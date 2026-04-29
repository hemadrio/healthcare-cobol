/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Stored Procedure: SP_ELIGIBILITY_CHECK
 *
 * Purpose  : Real-time eligibility verification. Returns complete eligibility
 *            status, plan information, benefit details, accumulators,
 *            COB information, PCP assignment, and retroactive termination data.
 *
 * Database : HCPS_DB
 * Platform : Sybase ASE
 * Version  : 1.0
 *
 * Parameters:
 *   @p_member_id      - Member/subscriber ID to look up
 *   @p_dos            - Date of service for eligibility check
 *   @p_payer_id       - Optional payer ID (null = all payers)
 *   @p_service_type   - Optional service type code for benefit-specific check
 *   @p_return_code    - Output: 0=Eligible, 1=Not Eligible, -1=Error
 *   @p_return_msg     - Output: Status message
 ******************************************************************************/

use HCPS_DB
go

if exists (select 1 from sysobjects where name = 'SP_ELIGIBILITY_CHECK' and type = 'P')
    drop procedure SP_ELIGIBILITY_CHECK
go

create procedure SP_ELIGIBILITY_CHECK
    @p_member_id      varchar(20),
    @p_dos            datetime,
    @p_payer_id       varchar(10) = null,
    @p_service_type   varchar(5)  = null,
    @p_return_code    int          output,
    @p_return_msg     varchar(255) output
as
begin
    /* -------------------------------------------------------------------
     * Local variable declarations
     * ---------------------------------------------------------------- */
    declare @v_member_found        char(1)
    declare @v_elig_found          char(1)
    declare @v_elig_count          int
    declare @v_pat_status          char(1)
    declare @v_pat_last_name       varchar(35)
    declare @v_pat_first_name      varchar(25)
    declare @v_pat_dob             datetime
    declare @v_pat_gender          char(1)
    declare @v_pat_ssn             varchar(11)
    declare @v_pat_relationship    char(2)
    declare @v_pat_subscriber_id   varchar(20)
    declare @v_pat_group_id        varchar(15)
    declare @v_pat_plan_id         varchar(15)
    declare @v_pat_pcp_prov_id     varchar(15)
    declare @v_pat_pcp_npi         char(10)
    declare @v_pat_cob_flag        char(1)
    declare @v_pat_cob_payer_id    varchar(10)
    declare @v_pat_cob_order       char(1)
    declare @v_pat_medicare_id     varchar(15)
    declare @v_pat_medicaid_id     varchar(15)

    declare @v_elig_payer_id       varchar(10)
    declare @v_elig_status         char(1)
    declare @v_elig_plan_id        varchar(15)
    declare @v_elig_subplan_id     varchar(10)
    declare @v_elig_group_id       varchar(15)
    declare @v_elig_class_code     varchar(10)
    declare @v_elig_subscriber_id  varchar(20)
    declare @v_elig_relationship   char(2)
    declare @v_elig_benefit_pkg    varchar(15)
    declare @v_elig_network_id     varchar(15)
    declare @v_elig_eff_date       datetime
    declare @v_elig_term_date      datetime
    declare @v_elig_term_reason    varchar(10)
    declare @v_elig_pcp_required   char(1)
    declare @v_elig_pcp_prov_id    varchar(15)
    declare @v_elig_referral_req   char(1)
    declare @v_elig_preauth_req    char(1)
    declare @v_elig_cob_order      char(1)
    declare @v_elig_cob_payer_id   varchar(10)
    declare @v_elig_cob_policy     varchar(30)
    declare @v_elig_retro_flag     char(1)
    declare @v_elig_retro_eff      datetime
    declare @v_elig_cobra_flag     char(1)
    declare @v_elig_medicare_flag  char(1)
    declare @v_elig_mh_parity      char(1)
    declare @v_elig_rx_flag        char(1)
    declare @v_elig_dental_flag    char(1)
    declare @v_elig_vision_flag    char(1)
    declare @v_elig_plan_yr_start  datetime
    declare @v_elig_plan_yr_end    datetime

    /* Benefit amounts */
    declare @v_ind_deductible      numeric(13,2)
    declare @v_fam_deductible      numeric(13,2)
    declare @v_ind_oop_max         numeric(13,2)
    declare @v_fam_oop_max         numeric(13,2)
    declare @v_lifetime_max        numeric(13,2)
    declare @v_copay_pcp           numeric(13,2)
    declare @v_copay_spec          numeric(13,2)
    declare @v_copay_er            numeric(13,2)
    declare @v_copay_urgent        numeric(13,2)
    declare @v_copay_rx            numeric(13,2)
    declare @v_coins_in_pct        numeric(9,4)
    declare @v_coins_out_pct       numeric(9,4)

    /* Accumulator values */
    declare @v_ded_ind_used        numeric(13,2)
    declare @v_ded_ind_remain      numeric(13,2)
    declare @v_ded_fam_used        numeric(13,2)
    declare @v_ded_fam_remain      numeric(13,2)
    declare @v_oop_ind_used        numeric(13,2)
    declare @v_oop_ind_remain      numeric(13,2)
    declare @v_oop_fam_used        numeric(13,2)
    declare @v_oop_fam_remain      numeric(13,2)
    declare @v_ltm_used            numeric(13,2)
    declare @v_ltm_remain          numeric(13,2)

    /* PCP provider details */
    declare @v_pcp_prov_name       varchar(60)
    declare @v_pcp_prov_phone      varchar(15)
    declare @v_pcp_prov_specialty  varchar(10)
    declare @v_pcp_eff_date        datetime

    /* Retroactive check variables */
    declare @v_retro_term_found    char(1)
    declare @v_retro_term_date     datetime
    declare @v_retro_orig_term     datetime

    /* COB detail variables */
    declare @v_cob_other_payer_nm  varchar(60)
    declare @v_cob_other_type      char(2)

    declare @v_error_code          int
    declare @v_process_dt          datetime

    /* -------------------------------------------------------------------
     * Initialize
     * ---------------------------------------------------------------- */
    select @p_return_code      = 0
    select @p_return_msg       = ''
    select @v_member_found     = 'N'
    select @v_elig_found       = 'N'
    select @v_retro_term_found = 'N'
    select @v_process_dt       = getdate()

    /* -------------------------------------------------------------------
     * STEP 1: Validate member exists in Patient Master
     * ---------------------------------------------------------------- */
    select @v_pat_last_name     = PAT_LAST_NAME,
           @v_pat_first_name    = PAT_FIRST_NAME,
           @v_pat_dob           = PAT_DOB,
           @v_pat_gender        = PAT_GENDER,
           @v_pat_ssn           = PAT_SSN,
           @v_pat_status        = PAT_STATUS,
           @v_pat_relationship  = PAT_RELATIONSHIP,
           @v_pat_subscriber_id = PAT_SUBSCRIBER_ID,
           @v_pat_group_id      = PAT_GROUP_ID,
           @v_pat_plan_id       = PAT_PLAN_ID,
           @v_pat_pcp_prov_id   = PAT_PCP_PROVIDER_ID,
           @v_pat_pcp_npi       = PAT_PCP_NPI,
           @v_pat_cob_flag      = PAT_COB_FLAG,
           @v_pat_cob_payer_id  = PAT_COB_PAYER_ID,
           @v_pat_cob_order     = PAT_COB_ORDER,
           @v_pat_medicare_id   = PAT_MEDICARE_ID,
           @v_pat_medicaid_id   = PAT_MEDICAID_ID,
           @v_pcp_eff_date      = PAT_PCP_EFF_DATE
      from PATIENT_MASTER
     where PAT_MEMBER_ID = @p_member_id

    if @@rowcount = 0
    begin
        select @p_return_code = -1
        select @p_return_msg  = 'Member ID not found: ' + @p_member_id
        return -1
    end

    select @v_member_found = 'Y'

    /* Check if member record is deceased */
    if @v_pat_status = 'D'
    begin
        select @p_return_code = 1
        select @p_return_msg  = 'Member is deceased. Member ID: ' + @p_member_id
        /* Continue to return demographic info but flag as not eligible */
    end

    /* Check if member record is inactive */
    if @v_pat_status = 'I'
    begin
        select @p_return_code = 1
        select @p_return_msg  = 'Member record is inactive. Member ID: ' + @p_member_id
    end

    /* -------------------------------------------------------------------
     * STEP 2: Look up eligibility span for date of service
     * ---------------------------------------------------------------- */
    select @v_elig_payer_id      = ELIG_PAYER_ID,
           @v_elig_status        = ELIG_STATUS,
           @v_elig_plan_id       = ELIG_PLAN_ID,
           @v_elig_subplan_id    = ELIG_SUBPLAN_ID,
           @v_elig_group_id      = ELIG_GROUP_ID,
           @v_elig_class_code    = ELIG_CLASS_CODE,
           @v_elig_subscriber_id = ELIG_SUBSCRIBER_ID,
           @v_elig_relationship  = ELIG_RELATIONSHIP,
           @v_elig_benefit_pkg   = ELIG_BENEFIT_PKG,
           @v_elig_network_id    = ELIG_NETWORK_ID,
           @v_elig_eff_date      = ELIG_EFF_DATE,
           @v_elig_term_date     = ELIG_TERM_DATE,
           @v_elig_term_reason   = ELIG_TERM_REASON,
           @v_elig_pcp_required  = ELIG_PCP_REQUIRED,
           @v_elig_pcp_prov_id   = ELIG_PCP_PROVIDER_ID,
           @v_elig_referral_req  = ELIG_REFERRAL_REQ,
           @v_elig_preauth_req   = ELIG_PREAUTH_REQ,
           @v_elig_cob_order     = ELIG_COB_ORDER,
           @v_elig_cob_payer_id  = ELIG_COB_PAYER_ID,
           @v_elig_cob_policy    = ELIG_COB_POLICY_NO,
           @v_elig_retro_flag    = ELIG_RETRO_FLAG,
           @v_elig_retro_eff     = ELIG_RETRO_EFF_DATE,
           @v_elig_cobra_flag    = ELIG_COBRA_FLAG,
           @v_elig_medicare_flag = ELIG_MEDICARE_FLAG,
           @v_elig_mh_parity     = ELIG_MH_PARITY_FLAG,
           @v_elig_rx_flag       = ELIG_RX_BENEFIT_FLAG,
           @v_elig_dental_flag   = ELIG_DENTAL_FLAG,
           @v_elig_vision_flag   = ELIG_VISION_FLAG,
           @v_elig_plan_yr_start = ELIG_PLAN_YEAR_START,
           @v_elig_plan_yr_end   = ELIG_PLAN_YEAR_END,
           @v_ind_deductible     = isnull(ELIG_IND_DEDUCTIBLE, 0.00),
           @v_fam_deductible     = isnull(ELIG_FAM_DEDUCTIBLE, 0.00),
           @v_ind_oop_max        = isnull(ELIG_IND_OOP_MAX, 0.00),
           @v_fam_oop_max        = isnull(ELIG_FAM_OOP_MAX, 0.00),
           @v_lifetime_max       = isnull(ELIG_LIFETIME_MAX, 0.00),
           @v_copay_pcp          = isnull(ELIG_COPAY_PCP, 0.00),
           @v_copay_spec         = isnull(ELIG_COPAY_SPEC, 0.00),
           @v_copay_er           = isnull(ELIG_COPAY_ER, 0.00),
           @v_copay_urgent       = isnull(ELIG_COPAY_URGENT, 0.00),
           @v_copay_rx           = isnull(ELIG_COPAY_RX, 0.00),
           @v_coins_in_pct       = isnull(ELIG_COINS_IN_PCT, 0.0000),
           @v_coins_out_pct      = isnull(ELIG_COINS_OUT_PCT, 0.0000)
      from ELIGIBILITY
     where ELIG_MEMBER_ID = @p_member_id
       and (@p_payer_id is null or ELIG_PAYER_ID = @p_payer_id)
       and ELIG_EFF_DATE  <= @p_dos
       and (ELIG_TERM_DATE is null or ELIG_TERM_DATE >= @p_dos)
       and ELIG_STATUS = 'A'

    if @@rowcount = 0
    begin
        /* -----------------------------------------------------------
         * No active eligibility found. Check for recently terminated
         * spans that might indicate a retroactive termination.
         * -------------------------------------------------------- */
        select @v_elig_found = 'N'

        /* Check for retroactive terminations within 90 days */
        select @v_retro_term_date = ELIG_TERM_DATE,
               @v_retro_orig_term = ELIG_RETRO_EFF_DATE,
               @v_elig_payer_id   = ELIG_PAYER_ID,
               @v_elig_plan_id    = ELIG_PLAN_ID,
               @v_elig_group_id   = ELIG_GROUP_ID
          from ELIGIBILITY
         where ELIG_MEMBER_ID = @p_member_id
           and (@p_payer_id is null or ELIG_PAYER_ID = @p_payer_id)
           and ELIG_EFF_DATE  <= @p_dos
           and ELIG_TERM_DATE <  @p_dos
           and ELIG_RETRO_FLAG = 'Y'
           and datediff(day, ELIG_TERM_DATE, @p_dos) <= 90

        if @@rowcount > 0
        begin
            select @v_retro_term_found = 'Y'
            select @p_return_code = 1
            select @p_return_msg  = 'RETROACTIVE TERMINATION detected. ' +
                'Term date: ' + convert(varchar(10), @v_retro_term_date, 101) +
                '. Original term: ' +
                isnull(convert(varchar(10), @v_retro_orig_term, 101), 'N/A') +
                '. Payer: ' + @v_elig_payer_id +
                ', Plan: ' + @v_elig_plan_id
        end
        else
        begin
            /* Check for pending eligibility */
            select @v_elig_count = count(*)
              from ELIGIBILITY
             where ELIG_MEMBER_ID = @p_member_id
               and (@p_payer_id is null or ELIG_PAYER_ID = @p_payer_id)
               and ELIG_STATUS = 'P'

            if @v_elig_count > 0
            begin
                select @p_return_code = 1
                select @p_return_msg  = 'Eligibility status is PENDING for member: '
                    + @p_member_id + '. ' + convert(varchar(5), @v_elig_count)
                    + ' pending span(s) found.'
            end
            else
            begin
                select @p_return_code = 1
                select @p_return_msg  = 'No active eligibility found for member: '
                    + @p_member_id + ' on DOS: '
                    + convert(varchar(10), @p_dos, 101)
            end
        end
    end
    else
    begin
        select @v_elig_found = 'Y'
    end

    /* -------------------------------------------------------------------
     * STEP 3: Get PCP provider details
     * ---------------------------------------------------------------- */
    declare @v_pcp_id varchar(15)
    select @v_pcp_id = isnull(@v_elig_pcp_prov_id, @v_pat_pcp_prov_id)

    if @v_pcp_id is not null
    begin
        select @v_pcp_prov_name = case when PROV_TYPE = 'IN'
                    then PROV_LAST_NAME + ', ' + PROV_FIRST_NAME
                    else PROV_ORG_NAME end,
               @v_pcp_prov_phone     = PROV_PHONE,
               @v_pcp_prov_specialty = PROV_SPECIALTY_CODE
          from PROVIDER_MASTER
         where PROV_PROVIDER_ID = @v_pcp_id
           and PROV_STATUS = 'A'

        if @@rowcount = 0
        begin
            select @v_pcp_prov_name = 'PCP NOT FOUND OR INACTIVE'
        end
    end

    /* -------------------------------------------------------------------
     * STEP 4: Retrieve benefit accumulators
     * ---------------------------------------------------------------- */
    if @v_elig_found = 'Y'
    begin
        /* Individual deductible accumulator */
        select @v_ded_ind_used   = isnull(ACCUM_USED_AMT, 0.00),
               @v_ded_ind_remain = isnull(ACCUM_REMAIN_AMT, 0.00)
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @v_elig_payer_id
           and ACCUM_PLAN_ID      = @v_elig_plan_id
           and ACCUM_TYPE         = 'DED'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_PERIOD_START <= @p_dos
           and ACCUM_PERIOD_END   >= @p_dos

        if @@rowcount = 0
        begin
            select @v_ded_ind_used   = 0.00
            select @v_ded_ind_remain = @v_ind_deductible
        end

        /* Family deductible accumulator */
        select @v_ded_fam_used   = isnull(ACCUM_USED_AMT, 0.00),
               @v_ded_fam_remain = isnull(ACCUM_REMAIN_AMT, 0.00)
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @v_elig_payer_id
           and ACCUM_PLAN_ID      = @v_elig_plan_id
           and ACCUM_TYPE         = 'DED'
           and ACCUM_SCOPE        = 'F'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_PERIOD_START <= @p_dos
           and ACCUM_PERIOD_END   >= @p_dos

        if @@rowcount = 0
        begin
            select @v_ded_fam_used   = 0.00
            select @v_ded_fam_remain = @v_fam_deductible
        end

        /* Individual OOP accumulator */
        select @v_oop_ind_used   = isnull(ACCUM_USED_AMT, 0.00),
               @v_oop_ind_remain = isnull(ACCUM_REMAIN_AMT, 0.00)
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @v_elig_payer_id
           and ACCUM_PLAN_ID      = @v_elig_plan_id
           and ACCUM_TYPE         = 'OOP'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_PERIOD_START <= @p_dos
           and ACCUM_PERIOD_END   >= @p_dos

        if @@rowcount = 0
        begin
            select @v_oop_ind_used   = 0.00
            select @v_oop_ind_remain = @v_ind_oop_max
        end

        /* Family OOP accumulator */
        select @v_oop_fam_used   = isnull(ACCUM_USED_AMT, 0.00),
               @v_oop_fam_remain = isnull(ACCUM_REMAIN_AMT, 0.00)
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @v_elig_payer_id
           and ACCUM_PLAN_ID      = @v_elig_plan_id
           and ACCUM_TYPE         = 'OOP'
           and ACCUM_SCOPE        = 'F'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_PERIOD_START <= @p_dos
           and ACCUM_PERIOD_END   >= @p_dos

        if @@rowcount = 0
        begin
            select @v_oop_fam_used   = 0.00
            select @v_oop_fam_remain = @v_fam_oop_max
        end

        /* Lifetime maximum accumulator */
        select @v_ltm_used   = isnull(ACCUM_USED_AMT, 0.00),
               @v_ltm_remain = isnull(ACCUM_REMAIN_AMT, 0.00)
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID = @p_member_id
           and ACCUM_PAYER_ID  = @v_elig_payer_id
           and ACCUM_PLAN_ID   = @v_elig_plan_id
           and ACCUM_TYPE      = 'LTM'
           and ACCUM_SCOPE     = 'I'
           and ACCUM_NETWORK   = 'B'

        if @@rowcount = 0
        begin
            select @v_ltm_used   = 0.00
            select @v_ltm_remain = @v_lifetime_max
        end
    end

    /* -------------------------------------------------------------------
     * STEP 5: COB lookup - get other insurance information
     * ---------------------------------------------------------------- */
    declare @v_cob_other_payer_id varchar(10)
    select @v_cob_other_payer_id = isnull(@v_elig_cob_payer_id,
                                          @v_pat_cob_payer_id)

    if @v_cob_other_payer_id is not null
    begin
        select @v_cob_other_payer_nm = PAYER_NAME,
               @v_cob_other_type     = PAYER_TYPE
          from PAYER_MASTER
         where PAYER_ID = @v_cob_other_payer_id

        if @@rowcount = 0
        begin
            select @v_cob_other_payer_nm = 'OTHER PAYER NOT ON FILE'
        end
    end

    /* -------------------------------------------------------------------
     * STEP 6: Check for retroactive eligibility changes
     * ---------------------------------------------------------------- */
    if @v_elig_found = 'Y' and @v_retro_term_found = 'N'
    begin
        /* Check if there is a future retroactive termination pending */
        select @v_elig_count = count(*)
          from ELIGIBILITY e2
         where e2.ELIG_MEMBER_ID = @p_member_id
           and e2.ELIG_PAYER_ID  = @v_elig_payer_id
           and e2.ELIG_STATUS    = 'T'
           and e2.ELIG_RETRO_FLAG = 'Y'
           and e2.ELIG_EFF_DATE  <= @p_dos
           and e2.ELIG_TERM_DATE <  @p_dos

        if @v_elig_count > 0
        begin
            select @v_retro_term_found = 'Y'
        end
    end

    /* -------------------------------------------------------------------
     * STEP 7: Return result sets
     * ---------------------------------------------------------------- */

    /* Result Set 1: Member Demographics */
    select @p_member_id         as MEMBER_ID,
           @v_pat_last_name     as LAST_NAME,
           @v_pat_first_name    as FIRST_NAME,
           @v_pat_dob           as DATE_OF_BIRTH,
           @v_pat_gender        as GENDER,
           @v_pat_ssn           as SSN,
           @v_pat_status        as MEMBER_STATUS,
           @v_pat_relationship  as RELATIONSHIP_CODE,
           @v_pat_subscriber_id as SUBSCRIBER_ID,
           @v_pat_medicare_id   as MEDICARE_ID,
           @v_pat_medicaid_id   as MEDICAID_ID

    /* Result Set 2: Eligibility Details */
    if @v_elig_found = 'Y'
    begin
        select @v_elig_payer_id      as PAYER_ID,
               @v_elig_status        as ELIG_STATUS,
               @v_elig_plan_id       as PLAN_ID,
               @v_elig_subplan_id    as SUBPLAN_ID,
               @v_elig_group_id      as GROUP_ID,
               @v_elig_class_code    as CLASS_CODE,
               @v_elig_benefit_pkg   as BENEFIT_PACKAGE,
               @v_elig_network_id    as NETWORK_ID,
               @v_elig_eff_date      as ELIG_EFF_DATE,
               @v_elig_term_date     as ELIG_TERM_DATE,
               @v_elig_pcp_required  as PCP_REQUIRED,
               @v_elig_referral_req  as REFERRAL_REQUIRED,
               @v_elig_preauth_req   as PREAUTH_REQUIRED,
               @v_elig_cobra_flag    as COBRA_FLAG,
               @v_elig_medicare_flag as MEDICARE_FLAG,
               @v_elig_mh_parity     as MH_PARITY_FLAG,
               @v_elig_rx_flag       as RX_BENEFIT_FLAG,
               @v_elig_dental_flag   as DENTAL_BENEFIT_FLAG,
               @v_elig_vision_flag   as VISION_BENEFIT_FLAG,
               @v_elig_plan_yr_start as PLAN_YEAR_START,
               @v_elig_plan_yr_end   as PLAN_YEAR_END,
               @v_retro_term_found   as RETRO_TERM_FLAG
    end

    /* Result Set 3: Benefit Amounts */
    if @v_elig_found = 'Y'
    begin
        select @v_ind_deductible  as IND_DEDUCTIBLE,
               @v_fam_deductible  as FAM_DEDUCTIBLE,
               @v_ind_oop_max     as IND_OOP_MAX,
               @v_fam_oop_max     as FAM_OOP_MAX,
               @v_lifetime_max    as LIFETIME_MAX,
               @v_copay_pcp       as COPAY_PCP,
               @v_copay_spec      as COPAY_SPECIALIST,
               @v_copay_er        as COPAY_ER,
               @v_copay_urgent    as COPAY_URGENT_CARE,
               @v_copay_rx        as COPAY_RX,
               @v_coins_in_pct    as COINSURANCE_IN_NET_PCT,
               @v_coins_out_pct   as COINSURANCE_OUT_NET_PCT
    end

    /* Result Set 4: Accumulator Summary */
    if @v_elig_found = 'Y'
    begin
        select 'IND_DEDUCTIBLE'  as ACCUM_TYPE,
               @v_ind_deductible as LIMIT_AMT,
               @v_ded_ind_used   as USED_AMT,
               @v_ded_ind_remain as REMAIN_AMT
        union all
        select 'FAM_DEDUCTIBLE',
               @v_fam_deductible,
               @v_ded_fam_used,
               @v_ded_fam_remain
        union all
        select 'IND_OOP_MAX',
               @v_ind_oop_max,
               @v_oop_ind_used,
               @v_oop_ind_remain
        union all
        select 'FAM_OOP_MAX',
               @v_fam_oop_max,
               @v_oop_fam_used,
               @v_oop_fam_remain
        union all
        select 'LIFETIME_MAX',
               @v_lifetime_max,
               @v_ltm_used,
               @v_ltm_remain
    end

    /* Result Set 5: PCP Assignment */
    select @v_pcp_id              as PCP_PROVIDER_ID,
           isnull(@v_pat_pcp_npi, '') as PCP_NPI,
           @v_pcp_prov_name       as PCP_PROVIDER_NAME,
           @v_pcp_prov_phone      as PCP_PHONE,
           @v_pcp_prov_specialty  as PCP_SPECIALTY,
           @v_pcp_eff_date        as PCP_EFF_DATE,
           @v_elig_pcp_required   as PCP_REQUIRED_FLAG

    /* Result Set 6: COB Information */
    if @v_cob_other_payer_id is not null
    begin
        select isnull(@v_elig_cob_order, @v_pat_cob_order) as COB_ORDER,
               @v_cob_other_payer_id    as OTHER_PAYER_ID,
               @v_cob_other_payer_nm    as OTHER_PAYER_NAME,
               @v_cob_other_type        as OTHER_PAYER_TYPE,
               @v_elig_cob_policy       as OTHER_POLICY_NO,
               @v_pat_cob_flag          as COB_FLAG
    end

    /* -------------------------------------------------------------------
     * STEP 8: Set final return message if eligible
     * ---------------------------------------------------------------- */
    if @v_elig_found = 'Y' and @p_return_code = 0
    begin
        select @p_return_code = 0
        select @p_return_msg  = 'ELIGIBLE. Payer: ' + @v_elig_payer_id +
            ', Plan: ' + @v_elig_plan_id +
            ', Group: ' + isnull(@v_elig_group_id, 'N/A') +
            ', Eff: ' + convert(varchar(10), @v_elig_eff_date, 101) +
            case when @v_elig_term_date is not null
                 then ', Term: ' + convert(varchar(10), @v_elig_term_date, 101)
                 else '' end +
            case when @v_retro_term_found = 'Y'
                 then ' [RETRO TERM WARNING]'
                 else '' end +
            case when @v_cob_other_payer_id is not null
                 then ' [COB: ' + @v_cob_other_payer_id + ']'
                 else '' end
    end

    return @p_return_code
end
go

/* Grant execute permission */
grant execute on SP_ELIGIBILITY_CHECK to hcps_app_role
go

print 'SP_ELIGIBILITY_CHECK created successfully.'
go

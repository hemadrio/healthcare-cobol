/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Stored Procedure: SP_BENEFIT_ACCUM
 *
 * Purpose  : Benefit accumulator management for deductible, out-of-pocket,
 *            lifetime maximum, and annual benefit limit tracking.
 *            Handles individual and family accumulators, plan year resets,
 *            family deductible cross-accumulation, and concurrent claim
 *            accumulator reservation to prevent over-application.
 *
 * Database : HCPS_DB
 * Platform : Sybase ASE
 * Version  : 1.0
 *
 * Parameters:
 *   @p_member_id       - Member ID
 *   @p_payer_id        - Payer ID
 *   @p_plan_id         - Plan ID
 *   @p_claim_no        - Claim number for tracking
 *   @p_dos             - Date of service
 *   @p_deductible_amt  - Deductible amount to apply
 *   @p_copay_amt       - Copay amount to apply
 *   @p_coinsurance_amt - Coinsurance amount to apply
 *   @p_allowed_amt     - Total allowed amount (for lifetime tracking)
 *   @p_user_id         - Processing user ID
 *   @p_return_code     - Output: 0=Success, 1=Warning, -1=Error
 *   @p_return_msg      - Output: Status message
 ******************************************************************************/

use HCPS_DB
go

if exists (select 1 from sysobjects where name = 'SP_BENEFIT_ACCUM' and type = 'P')
    drop procedure SP_BENEFIT_ACCUM
go

create procedure SP_BENEFIT_ACCUM
    @p_member_id       varchar(20),
    @p_payer_id        varchar(10),
    @p_plan_id         varchar(15),
    @p_claim_no        varchar(15),
    @p_dos             datetime,
    @p_deductible_amt  numeric(13,2),
    @p_copay_amt       numeric(13,2),
    @p_coinsurance_amt numeric(13,2),
    @p_allowed_amt     numeric(13,2),
    @p_user_id         varchar(30),
    @p_return_code     int          output,
    @p_return_msg      varchar(255) output
as
begin
    /* -------------------------------------------------------------------
     * Local variable declarations
     * ---------------------------------------------------------------- */
    /* Accumulator tracking variables */
    declare @v_accum_exists        char(1)
    declare @v_accum_status        char(1)
    declare @v_current_used        numeric(13,2)
    declare @v_current_remain      numeric(13,2)
    declare @v_current_reserved    numeric(13,2)
    declare @v_current_limit       numeric(13,2)
    declare @v_new_used            numeric(13,2)
    declare @v_new_remain          numeric(13,2)
    declare @v_new_status          char(1)
    declare @v_total_pat_resp      numeric(13,2)

    /* Plan year variables */
    declare @v_plan_yr_start       datetime
    declare @v_plan_yr_end         datetime
    declare @v_benefit_year        smallint
    declare @v_needs_reset         char(1)

    /* Family accumulator variables */
    declare @v_fam_cross_id        varchar(20)
    declare @v_fam_ded_limit       numeric(13,2)
    declare @v_fam_ded_used        numeric(13,2)
    declare @v_fam_ded_remain      numeric(13,2)
    declare @v_fam_oop_limit       numeric(13,2)
    declare @v_fam_oop_used        numeric(13,2)
    declare @v_fam_oop_remain      numeric(13,2)
    declare @v_fam_member_count    smallint
    declare @v_fam_ded_satisfied   char(1)
    declare @v_fam_oop_satisfied   char(1)

    /* Concurrent reservation variables */
    declare @v_reservation_amt     numeric(13,2)
    declare @v_effective_remain    numeric(13,2)
    declare @v_concurrent_count    int

    /* Lifetime maximum variables */
    declare @v_ltm_limit           numeric(13,2)
    declare @v_ltm_used            numeric(13,2)
    declare @v_ltm_remain          numeric(13,2)
    declare @v_ltm_exceeded        char(1)

    /* Annual benefit limit variables */
    declare @v_anl_limit           numeric(13,2)
    declare @v_anl_used            numeric(13,2)
    declare @v_anl_remain          numeric(13,2)

    /* Eligibility benefit amounts */
    declare @v_elig_ind_ded        numeric(13,2)
    declare @v_elig_fam_ded        numeric(13,2)
    declare @v_elig_ind_oop        numeric(13,2)
    declare @v_elig_fam_oop        numeric(13,2)
    declare @v_elig_ltm            numeric(13,2)
    declare @v_subscriber_id       varchar(20)

    declare @v_error_code          int
    declare @v_process_dt          datetime
    declare @v_warnings            varchar(255)

    /* -------------------------------------------------------------------
     * Initialize
     * ---------------------------------------------------------------- */
    select @p_return_code    = 0
    select @p_return_msg     = ''
    select @v_process_dt     = getdate()
    select @v_needs_reset    = 'N'
    select @v_fam_ded_satisfied = 'N'
    select @v_fam_oop_satisfied = 'N'
    select @v_ltm_exceeded   = 'N'
    select @v_warnings       = ''
    select @v_total_pat_resp = @p_deductible_amt + @p_copay_amt +
                               @p_coinsurance_amt

    /* -------------------------------------------------------------------
     * STEP 1: Get plan year dates and benefit limits from eligibility
     * ---------------------------------------------------------------- */
    select @v_plan_yr_start  = ELIG_PLAN_YEAR_START,
           @v_plan_yr_end    = ELIG_PLAN_YEAR_END,
           @v_elig_ind_ded   = isnull(ELIG_IND_DEDUCTIBLE, 0.00),
           @v_elig_fam_ded   = isnull(ELIG_FAM_DEDUCTIBLE, 0.00),
           @v_elig_ind_oop   = isnull(ELIG_IND_OOP_MAX, 0.00),
           @v_elig_fam_oop   = isnull(ELIG_FAM_OOP_MAX, 0.00),
           @v_elig_ltm       = isnull(ELIG_LIFETIME_MAX, 9999999.99),
           @v_subscriber_id  = ELIG_SUBSCRIBER_ID
      from ELIGIBILITY
     where ELIG_MEMBER_ID = @p_member_id
       and ELIG_PAYER_ID  = @p_payer_id
       and ELIG_EFF_DATE  <= @p_dos
       and (ELIG_TERM_DATE is null or ELIG_TERM_DATE >= @p_dos)
       and ELIG_STATUS = 'A'

    if @@rowcount = 0
    begin
        select @p_return_code = 1
        select @p_return_msg  = 'WARNING: No active eligibility for accum update'
        return 1
    end

    /* Determine benefit year */
    if @v_plan_yr_start is not null
    begin
        select @v_benefit_year = datepart(year, @v_plan_yr_start)
    end
    else
    begin
        select @v_benefit_year = datepart(year, @p_dos)
        /* Default to calendar year */
        select @v_plan_yr_start = convert(datetime,
            convert(varchar(4), @v_benefit_year) + '-01-01')
        select @v_plan_yr_end   = convert(datetime,
            convert(varchar(4), @v_benefit_year) + '-12-31')
    end

    /* -------------------------------------------------------------------
     * STEP 2: Get family cross-accumulation ID
     * ---------------------------------------------------------------- */
    select @v_fam_cross_id = ACCUM_FAMILY_CROSS_ID
      from BENEFIT_ACCUMULATORS
     where ACCUM_MEMBER_ID    = @p_member_id
       and ACCUM_PAYER_ID     = @p_payer_id
       and ACCUM_PLAN_ID      = @p_plan_id
       and ACCUM_TYPE         = 'DED'
       and ACCUM_SCOPE        = 'I'
       and ACCUM_BENEFIT_YEAR = @v_benefit_year

    /* If no cross ID found, use subscriber ID as family link */
    if @v_fam_cross_id is null
    begin
        select @v_fam_cross_id = @v_subscriber_id
    end

    /* -------------------------------------------------------------------
     * STEP 3: Plan year reset check
     * ---------------------------------------------------------------- */
    /* Check if accumulators need to be reset for new plan year */
    select @v_accum_exists = 'N'

    select @v_accum_exists = 'Y',
           @v_accum_status = ACCUM_STATUS
      from BENEFIT_ACCUMULATORS
     where ACCUM_MEMBER_ID    = @p_member_id
       and ACCUM_PAYER_ID     = @p_payer_id
       and ACCUM_PLAN_ID      = @p_plan_id
       and ACCUM_TYPE         = 'DED'
       and ACCUM_SCOPE        = 'I'
       and ACCUM_NETWORK      = 'I'
       and ACCUM_BENEFIT_YEAR = @v_benefit_year

    if @v_accum_exists = 'N'
    begin
        /* Accumulators do not exist for this benefit year - create them */
        select @v_needs_reset = 'Y'
    end
    else if @v_accum_status = 'R'
    begin
        /* Already reset - proceed normally */
        select @v_needs_reset = 'N'
    end

    /* -------------------------------------------------------------------
     * STEP 4: Create new benefit year accumulators if needed
     * ---------------------------------------------------------------- */
    if @v_needs_reset = 'Y'
    begin
        begin transaction T_ACCUM_INIT

        /* Create individual in-network deductible accumulator */
        insert into BENEFIT_ACCUMULATORS
            (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
             ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
             ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
             ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
             ACCUM_RESERVED_AMT, ACCUM_PENDING_AMT,
             ACCUM_FAMILY_CROSS_ID, ACCUM_STATUS,
             ACCUM_ADD_DATE, ACCUM_ADD_USER)
        values
            (@p_member_id, @p_payer_id, @p_plan_id,
             'DED', 'I', 'I',
             @v_benefit_year, @v_plan_yr_start, @v_plan_yr_end,
             @v_elig_ind_ded, 0.00, @v_elig_ind_ded,
             0.00, 0.00,
             @v_fam_cross_id, 'A',
             @v_process_dt, @p_user_id)

        if @@error != 0
        begin
            rollback transaction T_ACCUM_INIT
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to create individual deductible accumulator'
            return -1
        end

        /* Create family in-network deductible accumulator */
        /* Only create if not already exists for this family */
        if not exists (
            select 1 from BENEFIT_ACCUMULATORS
             where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
               and ACCUM_PAYER_ID        = @p_payer_id
               and ACCUM_PLAN_ID         = @p_plan_id
               and ACCUM_TYPE            = 'DED'
               and ACCUM_SCOPE           = 'F'
               and ACCUM_BENEFIT_YEAR    = @v_benefit_year
        )
        begin
            insert into BENEFIT_ACCUMULATORS
                (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
                 ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
                 ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
                 ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
                 ACCUM_RESERVED_AMT, ACCUM_PENDING_AMT,
                 ACCUM_FAMILY_CROSS_ID, ACCUM_FAMILY_MEMBER_CT,
                 ACCUM_STATUS, ACCUM_ADD_DATE, ACCUM_ADD_USER)
            values
                (@p_member_id, @p_payer_id, @p_plan_id,
                 'DED', 'F', 'I',
                 @v_benefit_year, @v_plan_yr_start, @v_plan_yr_end,
                 @v_elig_fam_ded, 0.00, @v_elig_fam_ded,
                 0.00, 0.00,
                 @v_fam_cross_id, 1,
                 'A', @v_process_dt, @p_user_id)
        end

        /* Create individual OOP accumulator */
        insert into BENEFIT_ACCUMULATORS
            (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
             ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
             ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
             ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
             ACCUM_RESERVED_AMT, ACCUM_PENDING_AMT,
             ACCUM_FAMILY_CROSS_ID, ACCUM_STATUS,
             ACCUM_ADD_DATE, ACCUM_ADD_USER)
        values
            (@p_member_id, @p_payer_id, @p_plan_id,
             'OOP', 'I', 'I',
             @v_benefit_year, @v_plan_yr_start, @v_plan_yr_end,
             @v_elig_ind_oop, 0.00, @v_elig_ind_oop,
             0.00, 0.00,
             @v_fam_cross_id, 'A',
             @v_process_dt, @p_user_id)

        if @@error != 0
        begin
            rollback transaction T_ACCUM_INIT
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to create OOP accumulator'
            return -1
        end

        /* Create family OOP accumulator */
        if not exists (
            select 1 from BENEFIT_ACCUMULATORS
             where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
               and ACCUM_PAYER_ID        = @p_payer_id
               and ACCUM_PLAN_ID         = @p_plan_id
               and ACCUM_TYPE            = 'OOP'
               and ACCUM_SCOPE           = 'F'
               and ACCUM_BENEFIT_YEAR    = @v_benefit_year
        )
        begin
            insert into BENEFIT_ACCUMULATORS
                (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
                 ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
                 ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
                 ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
                 ACCUM_RESERVED_AMT, ACCUM_PENDING_AMT,
                 ACCUM_FAMILY_CROSS_ID, ACCUM_FAMILY_MEMBER_CT,
                 ACCUM_STATUS, ACCUM_ADD_DATE, ACCUM_ADD_USER)
            values
                (@p_member_id, @p_payer_id, @p_plan_id,
                 'OOP', 'F', 'I',
                 @v_benefit_year, @v_plan_yr_start, @v_plan_yr_end,
                 @v_elig_fam_oop, 0.00, @v_elig_fam_oop,
                 0.00, 0.00,
                 @v_fam_cross_id, 1,
                 'A', @v_process_dt, @p_user_id)
        end

        /* Create lifetime maximum accumulator (if not exists) */
        if not exists (
            select 1 from BENEFIT_ACCUMULATORS
             where ACCUM_MEMBER_ID = @p_member_id
               and ACCUM_PAYER_ID  = @p_payer_id
               and ACCUM_PLAN_ID   = @p_plan_id
               and ACCUM_TYPE      = 'LTM'
               and ACCUM_SCOPE     = 'I'
        )
        begin
            insert into BENEFIT_ACCUMULATORS
                (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
                 ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
                 ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
                 ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
                 ACCUM_RESERVED_AMT, ACCUM_FAMILY_CROSS_ID,
                 ACCUM_STATUS, ACCUM_ADD_DATE, ACCUM_ADD_USER)
            values
                (@p_member_id, @p_payer_id, @p_plan_id,
                 'LTM', 'I', 'B',
                 9999, '1900-01-01', '9999-12-31',
                 @v_elig_ltm, 0.00, @v_elig_ltm,
                 0.00, @v_fam_cross_id,
                 'A', @v_process_dt, @p_user_id)
        end

        /* Create annual benefit limit accumulator */
        insert into BENEFIT_ACCUMULATORS
            (ACCUM_MEMBER_ID, ACCUM_PAYER_ID, ACCUM_PLAN_ID,
             ACCUM_TYPE, ACCUM_SCOPE, ACCUM_NETWORK,
             ACCUM_BENEFIT_YEAR, ACCUM_PERIOD_START, ACCUM_PERIOD_END,
             ACCUM_LIMIT_AMT, ACCUM_USED_AMT, ACCUM_REMAIN_AMT,
             ACCUM_RESERVED_AMT, ACCUM_FAMILY_CROSS_ID,
             ACCUM_STATUS, ACCUM_ADD_DATE, ACCUM_ADD_USER)
        values
            (@p_member_id, @p_payer_id, @p_plan_id,
             'ANL', 'I', 'I',
             @v_benefit_year, @v_plan_yr_start, @v_plan_yr_end,
             9999999.99, 0.00, 9999999.99,
             0.00, @v_fam_cross_id,
             'A', @v_process_dt, @p_user_id)

        if @@error != 0
        begin
            rollback transaction T_ACCUM_INIT
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to create annual benefit accumulator'
            return -1
        end

        commit transaction T_ACCUM_INIT
    end

    /* -------------------------------------------------------------------
     * STEP 5: Handle concurrent claim accumulator reservation
     * ---------------------------------------------------------------- */
    /* Check for other claims currently being processed for this member
       that have reserved accumulator amounts */
    select @v_concurrent_count = count(*),
           @v_reservation_amt  = isnull(sum(ACCUM_RESERVED_AMT), 0.00)
      from BENEFIT_ACCUMULATORS
     where ACCUM_MEMBER_ID    = @p_member_id
       and ACCUM_PAYER_ID     = @p_payer_id
       and ACCUM_PLAN_ID      = @p_plan_id
       and ACCUM_TYPE         = 'DED'
       and ACCUM_SCOPE        = 'I'
       and ACCUM_NETWORK      = 'I'
       and ACCUM_BENEFIT_YEAR = @v_benefit_year
       and ACCUM_RESERVED_AMT > 0.00

    /* Reserve this claim's deductible amount to prevent over-application */
    begin transaction T_ACCUM_UPDATE

    if @p_deductible_amt > 0.00
    begin
        /* Update individual deductible - reserve first */
        update BENEFIT_ACCUMULATORS
           set ACCUM_RESERVED_AMT = isnull(ACCUM_RESERVED_AMT, 0.00) +
                                    @p_deductible_amt,
               ACCUM_CHG_DATE     = @v_process_dt,
               ACCUM_CHG_USER     = @p_user_id
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @p_payer_id
           and ACCUM_PLAN_ID      = @p_plan_id
           and ACCUM_TYPE         = 'DED'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_BENEFIT_YEAR = @v_benefit_year

        if @@error != 0
        begin
            rollback transaction T_ACCUM_UPDATE
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to reserve deductible accumulator'
            return -1
        end
    end

    /* -------------------------------------------------------------------
     * STEP 6: Update individual deductible accumulator
     * ---------------------------------------------------------------- */
    if @p_deductible_amt > 0.00
    begin
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @p_deductible_amt,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @p_deductible_amt),
               ACCUM_RESERVED_AMT  = case when ACCUM_RESERVED_AMT >= @p_deductible_amt
                                          then ACCUM_RESERVED_AMT - @p_deductible_amt
                                          else 0.00 end,
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @p_deductible_amt)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'  /* Satisfied */
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @p_payer_id
           and ACCUM_PLAN_ID      = @p_plan_id
           and ACCUM_TYPE         = 'DED'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_BENEFIT_YEAR = @v_benefit_year

        if @@error != 0
        begin
            rollback transaction T_ACCUM_UPDATE
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to update individual deductible'
            return -1
        end

        /* -----------------------------------------------------------
         * STEP 6a: Family deductible cross-accumulation
         * -------------------------------------------------------- */
        /* Update family deductible for all family members */
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @p_deductible_amt,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @p_deductible_amt),
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @p_deductible_amt)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
           and ACCUM_PAYER_ID        = @p_payer_id
           and ACCUM_PLAN_ID         = @p_plan_id
           and ACCUM_TYPE            = 'DED'
           and ACCUM_SCOPE           = 'F'
           and ACCUM_BENEFIT_YEAR    = @v_benefit_year

        if @@error != 0
        begin
            /* Non-fatal for family accumulator */
            select @v_warnings = @v_warnings + 'Family DED update failed; '
        end

        /* Check if family deductible is now satisfied */
        select @v_fam_ded_remain = ACCUM_REMAIN_AMT
          from BENEFIT_ACCUMULATORS
         where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
           and ACCUM_PAYER_ID        = @p_payer_id
           and ACCUM_PLAN_ID         = @p_plan_id
           and ACCUM_TYPE            = 'DED'
           and ACCUM_SCOPE           = 'F'
           and ACCUM_BENEFIT_YEAR    = @v_benefit_year

        if @v_fam_ded_remain is not null and @v_fam_ded_remain <= 0.00
        begin
            select @v_fam_ded_satisfied = 'Y'

            /* When family deductible is met, satisfy all individual
               member deductibles in the family */
            update BENEFIT_ACCUMULATORS
               set ACCUM_STATUS   = 'S',
                   ACCUM_CHG_DATE = @v_process_dt,
                   ACCUM_CHG_USER = @p_user_id
             where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
               and ACCUM_PAYER_ID        = @p_payer_id
               and ACCUM_PLAN_ID         = @p_plan_id
               and ACCUM_TYPE            = 'DED'
               and ACCUM_SCOPE           = 'I'
               and ACCUM_BENEFIT_YEAR    = @v_benefit_year
               and ACCUM_STATUS          = 'A'
        end
    end

    /* -------------------------------------------------------------------
     * STEP 7: Update OOP maximum accumulator
     * ---------------------------------------------------------------- */
    if @v_total_pat_resp > 0.00
    begin
        /* Update individual OOP */
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @v_total_pat_resp,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @v_total_pat_resp),
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @v_total_pat_resp)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @p_payer_id
           and ACCUM_PLAN_ID      = @p_plan_id
           and ACCUM_TYPE         = 'OOP'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_NETWORK      = 'I'
           and ACCUM_BENEFIT_YEAR = @v_benefit_year

        if @@error != 0
        begin
            rollback transaction T_ACCUM_UPDATE
            select @p_return_code = -1
            select @p_return_msg  = 'ERROR: Failed to update individual OOP'
            return -1
        end

        /* Update family OOP */
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @v_total_pat_resp,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @v_total_pat_resp),
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @v_total_pat_resp)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
           and ACCUM_PAYER_ID        = @p_payer_id
           and ACCUM_PLAN_ID         = @p_plan_id
           and ACCUM_TYPE            = 'OOP'
           and ACCUM_SCOPE           = 'F'
           and ACCUM_BENEFIT_YEAR    = @v_benefit_year

        if @@error != 0
        begin
            select @v_warnings = @v_warnings + 'Family OOP update failed; '
        end

        /* Check if family OOP is satisfied */
        select @v_fam_oop_remain = ACCUM_REMAIN_AMT
          from BENEFIT_ACCUMULATORS
         where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
           and ACCUM_PAYER_ID        = @p_payer_id
           and ACCUM_PLAN_ID         = @p_plan_id
           and ACCUM_TYPE            = 'OOP'
           and ACCUM_SCOPE           = 'F'
           and ACCUM_BENEFIT_YEAR    = @v_benefit_year

        if @v_fam_oop_remain is not null and @v_fam_oop_remain <= 0.00
        begin
            select @v_fam_oop_satisfied = 'Y'

            /* Satisfy all individual OOP accumulators in family */
            update BENEFIT_ACCUMULATORS
               set ACCUM_STATUS   = 'S',
                   ACCUM_CHG_DATE = @v_process_dt,
                   ACCUM_CHG_USER = @p_user_id
             where ACCUM_FAMILY_CROSS_ID = @v_fam_cross_id
               and ACCUM_PAYER_ID        = @p_payer_id
               and ACCUM_PLAN_ID         = @p_plan_id
               and ACCUM_TYPE            = 'OOP'
               and ACCUM_SCOPE           = 'I'
               and ACCUM_BENEFIT_YEAR    = @v_benefit_year
               and ACCUM_STATUS          = 'A'
        end
    end

    /* -------------------------------------------------------------------
     * STEP 8: Update lifetime maximum accumulator
     * ---------------------------------------------------------------- */
    if @p_allowed_amt > 0.00
    begin
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @p_allowed_amt,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @p_allowed_amt),
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @p_allowed_amt)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_MEMBER_ID = @p_member_id
           and ACCUM_PAYER_ID  = @p_payer_id
           and ACCUM_PLAN_ID   = @p_plan_id
           and ACCUM_TYPE      = 'LTM'
           and ACCUM_SCOPE     = 'I'

        if @@error != 0
        begin
            select @v_warnings = @v_warnings + 'Lifetime max update failed; '
        end

        /* Check if lifetime max was exceeded */
        select @v_ltm_remain = ACCUM_REMAIN_AMT
          from BENEFIT_ACCUMULATORS
         where ACCUM_MEMBER_ID = @p_member_id
           and ACCUM_PAYER_ID  = @p_payer_id
           and ACCUM_PLAN_ID   = @p_plan_id
           and ACCUM_TYPE      = 'LTM'
           and ACCUM_SCOPE     = 'I'

        if @v_ltm_remain is not null and @v_ltm_remain <= 0.00
        begin
            select @v_ltm_exceeded = 'Y'
            select @v_warnings = @v_warnings + 'LIFETIME MAX EXCEEDED; '
        end
    end

    /* -------------------------------------------------------------------
     * STEP 9: Update annual benefit limit accumulator
     * ---------------------------------------------------------------- */
    if @p_allowed_amt > 0.00
    begin
        update BENEFIT_ACCUMULATORS
           set ACCUM_USED_AMT      = ACCUM_USED_AMT + @p_allowed_amt,
               ACCUM_REMAIN_AMT    = ACCUM_LIMIT_AMT -
                                     (ACCUM_USED_AMT + @p_allowed_amt),
               ACCUM_LAST_CLAIM_NO = @p_claim_no,
               ACCUM_LAST_CLAIM_DATE = @p_dos,
               ACCUM_STATUS        = case when (ACCUM_USED_AMT + @p_allowed_amt)
                                              >= ACCUM_LIMIT_AMT
                                          then 'S'
                                          else 'A' end,
               ACCUM_CHG_DATE      = @v_process_dt,
               ACCUM_CHG_USER      = @p_user_id
         where ACCUM_MEMBER_ID    = @p_member_id
           and ACCUM_PAYER_ID     = @p_payer_id
           and ACCUM_PLAN_ID      = @p_plan_id
           and ACCUM_TYPE         = 'ANL'
           and ACCUM_SCOPE        = 'I'
           and ACCUM_BENEFIT_YEAR = @v_benefit_year

        if @@error != 0
        begin
            select @v_warnings = @v_warnings + 'Annual limit update failed; '
        end
    end

    /* -------------------------------------------------------------------
     * STEP 10: Commit transaction
     * ---------------------------------------------------------------- */
    commit transaction T_ACCUM_UPDATE

    /* -------------------------------------------------------------------
     * STEP 11: Build return message
     * ---------------------------------------------------------------- */
    /* Retrieve final accumulator state for return message */
    select @v_current_used   = ACCUM_USED_AMT,
           @v_current_remain = ACCUM_REMAIN_AMT
      from BENEFIT_ACCUMULATORS
     where ACCUM_MEMBER_ID    = @p_member_id
       and ACCUM_PAYER_ID     = @p_payer_id
       and ACCUM_PLAN_ID      = @p_plan_id
       and ACCUM_TYPE         = 'DED'
       and ACCUM_SCOPE        = 'I'
       and ACCUM_NETWORK      = 'I'
       and ACCUM_BENEFIT_YEAR = @v_benefit_year

    if @v_warnings != ''
    begin
        select @p_return_code = 1
        select @p_return_msg  = 'WARNINGS: ' + @v_warnings +
            ' | DED used: $' + convert(varchar(15), isnull(@v_current_used, 0.00)) +
            ', remain: $' + convert(varchar(15), isnull(@v_current_remain, 0.00))
    end
    else
    begin
        select @p_return_code = 0
        select @p_return_msg  = 'Accumulators updated. Claim: ' + @p_claim_no +
            ', DED applied: $' + convert(varchar(15), @p_deductible_amt) +
            ', OOP applied: $' + convert(varchar(15), @v_total_pat_resp) +
            case when @v_fam_ded_satisfied = 'Y'
                 then ', FAMILY DED SATISFIED'
                 else '' end +
            case when @v_fam_oop_satisfied = 'Y'
                 then ', FAMILY OOP SATISFIED'
                 else '' end +
            case when @v_ltm_exceeded = 'Y'
                 then ', LIFETIME MAX EXCEEDED'
                 else '' end
    end

    return @p_return_code
end
go

/* Grant execute permission */
grant execute on SP_BENEFIT_ACCUM to hcps_app_role
go

print 'SP_BENEFIT_ACCUM created successfully.'
go

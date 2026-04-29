/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Stored Procedure: SP_ADJUDICATE_CLAIM
 *
 * Database:    HCPS_PROD
 * Version:     4.2.0
 * Created:     2024-02-01
 * Modified:    2026-04-20
 * Author:      HCPS Claims Engine Team
 *
 * Description: Master claim adjudication procedure. Performs full pricing,
 *              benefit application, COB calculation, provider withhold,
 *              prompt pay interest, and final payment determination.
 *
 * Parameters:
 *   @claim_number       - Claim identifier
 *   @claim_suffix       - Claim suffix (00=original, 01+=adjustments)
 *   @adjudication_mode  - FULL, REPRICE, REAPPLY, OVERRIDE
 *   @override_user      - User ID for override processing
 *   @o_total_paid_amt   - OUTPUT: total payment amount
 *   @o_total_allowed_amt- OUTPUT: total allowed amount
 *   @o_total_deduct_amt - OUTPUT: total deductible applied
 *   @o_total_copay_amt  - OUTPUT: total copay applied
 *   @o_total_coins_amt  - OUTPUT: total coinsurance applied
 *   @o_total_cob_amt    - OUTPUT: total COB adjustment
 *   @o_total_withhold   - OUTPUT: total provider withhold
 *   @o_total_interest   - OUTPUT: total prompt pay interest
 *   @o_return_code      - OUTPUT: 0=success, >0=error/pend
 *   @o_return_message   - OUTPUT: result description
 *
 * CHANGE LOG:
 *   2024-02-01  Initial creation
 *   2024-08-15  Added per diem step-down with ICU differential
 *   2025-03-01  Added concurrent accumulator reservation
 *   2025-11-10  Added prompt pay interest calculation
 *   2026-04-20  Enhanced COB secondary payment logic
 ******************************************************************************/

USE HCPS_PROD
go

IF OBJECT_ID('dbo.SP_ADJUDICATE_CLAIM') IS NOT NULL
    DROP PROCEDURE dbo.SP_ADJUDICATE_CLAIM
go

CREATE PROCEDURE dbo.SP_ADJUDICATE_CLAIM
    @claim_number       varchar(15),
    @claim_suffix       varchar(2),
    @adjudication_mode  varchar(10) = 'FULL',
    @override_user      varchar(30) = NULL,
    @o_total_paid_amt   decimal(13,2) OUTPUT,
    @o_total_allowed_amt decimal(13,2) OUTPUT,
    @o_total_deduct_amt decimal(13,2) OUTPUT,
    @o_total_copay_amt  decimal(13,2) OUTPUT,
    @o_total_coins_amt  decimal(13,2) OUTPUT,
    @o_total_cob_amt    decimal(13,2) OUTPUT,
    @o_total_withhold   decimal(13,2) OUTPUT,
    @o_total_interest   decimal(13,2) OUTPUT,
    @o_return_code      int OUTPUT,
    @o_return_message   varchar(255) OUTPUT
AS
BEGIN
    SET NOCOUNT ON

    /***************************************************************************
     * LOCAL VARIABLE DECLARATIONS
     ***************************************************************************/
    DECLARE @err_code           int
    DECLARE @row_cnt            int
    DECLARE @tran_active        int
    DECLARE @step_name          varchar(50)

    /* Claim header variables */
    DECLARE @claim_type_cd      varchar(3)
    DECLARE @claim_status_cd    varchar(5)
    DECLARE @member_id          varchar(15)
    DECLARE @member_suffix      varchar(3)
    DECLARE @subscriber_id      varchar(15)
    DECLARE @payer_id           varchar(15)
    DECLARE @plan_cd            varchar(10)
    DECLARE @group_nbr          varchar(15)
    DECLARE @lob_cd             varchar(5)
    DECLARE @network_id         varchar(10)
    DECLARE @rendering_prov_id  varchar(15)
    DECLARE @billing_prov_id    varchar(15)
    DECLARE @facility_id        varchar(15)
    DECLARE @par_flag           char(1)
    DECLARE @network_tier_cd    varchar(3)
    DECLARE @place_of_service   varchar(2)
    DECLARE @bill_type_cd       varchar(4)
    DECLARE @admission_dt       datetime
    DECLARE @discharge_dt       datetime
    DECLARE @los_days           smallint
    DECLARE @drg_cd             varchar(5)
    DECLARE @drg_weight         decimal(9,4)
    DECLARE @drg_soi_cd         varchar(2)
    DECLARE @principal_diag_cd  varchar(8)
    DECLARE @receipt_dt         datetime
    DECLARE @total_charge_amt   decimal(13,2)
    DECLARE @cob_cd             varchar(3)
    DECLARE @other_payer_paid   decimal(13,2)

    /* Eligibility variables */
    DECLARE @elig_found         int
    DECLARE @elig_eff_dt        datetime
    DECLARE @elig_term_dt       datetime
    DECLARE @benefit_pkg_cd     varchar(10)
    DECLARE @coverage_type_cd   varchar(5)
    DECLARE @product_type_cd    varchar(5)
    DECLARE @ind_deductible     decimal(13,2)
    DECLARE @fam_deductible     decimal(13,2)
    DECLARE @oon_ind_deductible decimal(13,2)
    DECLARE @oon_fam_deductible decimal(13,2)
    DECLARE @ind_oop_max        decimal(13,2)
    DECLARE @fam_oop_max        decimal(13,2)
    DECLARE @oon_ind_oop_max    decimal(13,2)
    DECLARE @oon_fam_oop_max    decimal(13,2)
    DECLARE @coinsurance_pct    decimal(7,4)
    DECLARE @oon_coinsurance    decimal(7,4)
    DECLARE @er_copay           decimal(13,2)
    DECLARE @specialist_copay   decimal(13,2)
    DECLARE @pcp_copay          decimal(13,2)
    DECLARE @embedded_ded_flag  char(1)
    DECLARE @embedded_oop_flag  char(1)
    DECLARE @plan_year_start    datetime
    DECLARE @plan_year_end      datetime
    DECLARE @lifetime_max       decimal(13,2)
    DECLARE @grandfathered_flag char(1)

    /* Provider variables */
    DECLARE @prov_specialty     varchar(10)
    DECLARE @prov_locality      varchar(5)
    DECLARE @prov_gpci_locality varchar(5)
    DECLARE @prov_sanction_flag char(1)
    DECLARE @prov_contract_id   varchar(15)
    DECLARE @prov_withhold_pct  decimal(7,4)
    DECLARE @prov_capitated     char(1)
    DECLARE @wage_index_cbsa    varchar(7)
    DECLARE @teaching_hosp_flag char(1)
    DECLARE @dsh_pct            decimal(7,4)
    DECLARE @ime_ratio          decimal(7,4)

    /* Pricing variables */
    DECLARE @pricing_method     varchar(10)
    DECLARE @line_nbr           smallint
    DECLARE @line_proc_cd       varchar(7)
    DECLARE @line_modifier_1    varchar(2)
    DECLARE @line_modifier_2    varchar(2)
    DECLARE @line_charge_amt    decimal(13,2)
    DECLARE @line_units         decimal(9,3)
    DECLARE @line_dos_from      datetime
    DECLARE @line_dos_to        datetime
    DECLARE @line_pos_cd        varchar(2)
    DECLARE @line_revenue_cd    varchar(4)
    DECLARE @line_allowed_amt   decimal(13,2)

    /* DRG pricing variables */
    DECLARE @drg_base_rate      decimal(13,2)
    DECLARE @drg_adj_weight     decimal(9,4)
    DECLARE @drg_labor_share    decimal(7,4)
    DECLARE @drg_nonlabor_share decimal(7,4)
    DECLARE @drg_wage_index     decimal(9,6)
    DECLARE @drg_dsh_payment    decimal(13,2)
    DECLARE @drg_ime_payment    decimal(13,2)
    DECLARE @drg_outlier_amt    decimal(13,2)
    DECLARE @drg_total_payment  decimal(13,2)
    DECLARE @drg_cost_to_chrg   decimal(9,6)
    DECLARE @drg_outlier_thresh decimal(13,2)
    DECLARE @drg_operating_amt  decimal(13,2)
    DECLARE @drg_capital_amt    decimal(13,2)

    /* Per diem variables */
    DECLARE @perdiem_day1_rate  decimal(13,2)
    DECLARE @perdiem_day2_5     decimal(13,2)
    DECLARE @perdiem_day6_plus  decimal(13,2)
    DECLARE @perdiem_icu_rate   decimal(13,2)
    DECLARE @perdiem_total      decimal(13,2)
    DECLARE @perdiem_day_count  smallint

    /* RBRVS variables */
    DECLARE @work_rvu           decimal(9,4)
    DECLARE @pe_rvu             decimal(9,4)
    DECLARE @mp_rvu             decimal(9,4)
    DECLARE @work_gpci          decimal(9,5)
    DECLARE @pe_gpci            decimal(9,5)
    DECLARE @mp_gpci            decimal(9,5)
    DECLARE @conversion_factor  decimal(13,2)
    DECLARE @rbrvs_allowed      decimal(13,2)

    /* Accumulator variables */
    DECLARE @deduct_applied     decimal(13,2)
    DECLARE @deduct_remaining   decimal(13,2)
    DECLARE @deduct_met_flag    char(1)
    DECLARE @fam_deduct_applied decimal(13,2)
    DECLARE @fam_deduct_met     char(1)
    DECLARE @fam_mbr_ded_cnt    int
    DECLARE @oop_applied        decimal(13,2)
    DECLARE @oop_remaining      decimal(13,2)
    DECLARE @oop_met_flag       char(1)
    DECLARE @lifetime_used      decimal(13,2)
    DECLARE @lifetime_remaining decimal(13,2)

    /* COB variables */
    DECLARE @cob_other_allowed  decimal(13,2)
    DECLARE @cob_other_paid     decimal(13,2)
    DECLARE @cob_secondary_pay  decimal(13,2)
    DECLARE @cob_method_cd      varchar(10)
    DECLARE @cob_savings        decimal(13,2)

    /* Interest / prompt pay */
    DECLARE @prompt_pay_days    smallint
    DECLARE @prompt_pay_pct     decimal(7,4)
    DECLARE @calendar_days      int
    DECLARE @clean_claim_days   smallint

    /* Loop counter */
    DECLARE @max_line           smallint
    DECLARE @current_line       smallint

    /***************************************************************************
     * INITIALIZATION
     ***************************************************************************/
    SELECT @o_total_paid_amt   = 0.00,
           @o_total_allowed_amt = 0.00,
           @o_total_deduct_amt = 0.00,
           @o_total_copay_amt  = 0.00,
           @o_total_coins_amt  = 0.00,
           @o_total_cob_amt    = 0.00,
           @o_total_withhold   = 0.00,
           @o_total_interest   = 0.00,
           @o_return_code      = 0,
           @o_return_message   = 'SUCCESS',
           @tran_active        = 0,
           @step_name          = 'INIT'

    /***************************************************************************
     * CREATE TEMP TABLES FOR INTERMEDIATE CALCULATIONS
     ***************************************************************************/
    CREATE TABLE #line_pricing (
        line_nbr            smallint        NOT NULL,
        proc_cd             varchar(7)      NULL,
        modifier_1          varchar(2)      NULL,
        modifier_2          varchar(2)      NULL,
        revenue_cd          varchar(4)      NULL,
        dos_from            datetime        NULL,
        dos_to              datetime        NULL,
        pos_cd              varchar(2)      NULL,
        charge_amt          decimal(13,2)   NOT NULL DEFAULT 0.00,
        units               decimal(9,3)    NOT NULL DEFAULT 1.000,
        allowed_amt         decimal(13,2)   NOT NULL DEFAULT 0.00,
        deductible_amt      decimal(13,2)   NOT NULL DEFAULT 0.00,
        copay_amt           decimal(13,2)   NOT NULL DEFAULT 0.00,
        coinsurance_amt     decimal(13,2)   NOT NULL DEFAULT 0.00,
        cob_amt             decimal(13,2)   NOT NULL DEFAULT 0.00,
        withhold_amt        decimal(13,2)   NOT NULL DEFAULT 0.00,
        interest_amt        decimal(13,2)   NOT NULL DEFAULT 0.00,
        paid_amt            decimal(13,2)   NOT NULL DEFAULT 0.00,
        patient_resp_amt    decimal(13,2)   NOT NULL DEFAULT 0.00,
        pricing_method      varchar(10)     NULL,
        fee_schedule_id     varchar(15)     NULL,
        work_rvu            decimal(9,4)    NULL,
        pe_rvu              decimal(9,4)    NULL,
        mp_rvu              decimal(9,4)    NULL,
        multi_proc_rank     smallint        NULL,
        multi_proc_pct      decimal(7,4)    NULL,
        bilateral_flag      char(1)         NOT NULL DEFAULT 'N',
        assistant_flag      char(1)         NOT NULL DEFAULT 'N',
        line_status         varchar(5)      NOT NULL DEFAULT 'PROC',
        denial_reason       varchar(10)     NULL,
        carc_cd             varchar(5)      NULL,
        rarc_cd             varchar(5)      NULL
    )

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9001, @o_return_message = 'ERROR: Failed to create #line_pricing temp table'
        RETURN @o_return_code
    END

    CREATE TABLE #accum_work (
        accum_type_cd       varchar(10)     NOT NULL,
        network_tier        varchar(3)      NOT NULL,
        accum_level         varchar(3)      NOT NULL,
        limit_amt           decimal(13,2)   NOT NULL DEFAULT 0.00,
        applied_amt         decimal(13,2)   NOT NULL DEFAULT 0.00,
        reserved_amt        decimal(13,2)   NOT NULL DEFAULT 0.00,
        remaining_amt       decimal(13,2)   NOT NULL DEFAULT 0.00,
        this_claim_amt      decimal(13,2)   NOT NULL DEFAULT 0.00,
        accum_id            numeric(12,0)   NULL
    )

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9002, @o_return_message = 'ERROR: Failed to create #accum_work temp table'
        RETURN @o_return_code
    END

    CREATE TABLE #family_accum (
        subscriber_id       varchar(15)     NOT NULL,
        member_id           varchar(15)     NOT NULL,
        member_suffix       varchar(3)      NOT NULL,
        ind_deduct_met      decimal(13,2)   NOT NULL DEFAULT 0.00,
        ind_deduct_flag     char(1)         NOT NULL DEFAULT 'N'
    )

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9003, @o_return_message = 'ERROR: Failed to create #family_accum temp table'
        RETURN @o_return_code
    END

    /***************************************************************************
     * STEP 1: RETRIEVE CLAIM HEADER
     ***************************************************************************/
    SELECT @step_name = 'GET_CLAIM_HEADER'

    SELECT @claim_type_cd       = claim_type_cd,
           @claim_status_cd     = claim_status_cd,
           @member_id           = member_id,
           @member_suffix       = member_suffix,
           @subscriber_id       = subscriber_id,
           @payer_id            = payer_id,
           @plan_cd             = plan_cd,
           @group_nbr           = group_nbr,
           @lob_cd              = lob_cd,
           @network_id          = network_id,
           @rendering_prov_id   = rendering_provider_id,
           @billing_prov_id     = billing_provider_id,
           @facility_id         = facility_id,
           @par_flag            = par_flag,
           @network_tier_cd     = network_tier_cd,
           @place_of_service    = place_of_service_cd,
           @bill_type_cd        = bill_type_cd,
           @admission_dt        = admission_dt,
           @discharge_dt        = discharge_dt,
           @los_days            = los_days,
           @drg_cd              = drg_cd,
           @drg_weight          = drg_weight,
           @drg_soi_cd          = drg_soi_cd,
           @principal_diag_cd   = principal_diag_cd,
           @receipt_dt          = receipt_dt,
           @total_charge_amt    = total_charge_amt,
           @cob_cd              = coordination_of_benefits_cd,
           @other_payer_paid    = other_payer_paid_amt
    FROM   dbo.CLAIM_HEADER (NOLOCK)
    WHERE  claim_number = @claim_number
    AND    claim_suffix = @claim_suffix

    SELECT @err_code = @@error, @row_cnt = @@rowcount
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 1001, @o_return_message = 'ERROR: Failed to read claim header'
        GOTO ERROR_EXIT
    END
    IF @row_cnt = 0
    BEGIN
        SELECT @o_return_code = 1002, @o_return_message = 'ERROR: Claim not found - ' + @claim_number + '/' + @claim_suffix
        GOTO ERROR_EXIT
    END

    /* Validate claim status allows adjudication */
    IF @claim_status_cd NOT IN ('RECD', 'PEND', 'RTRN') AND @adjudication_mode != 'OVERRIDE'
    BEGIN
        SELECT @o_return_code = 1003,
               @o_return_message = 'ERROR: Claim status ' + @claim_status_cd + ' does not allow adjudication'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 2: LOAD CLAIM DETAIL LINES INTO TEMP TABLE
     ***************************************************************************/
    SELECT @step_name = 'LOAD_LINES'

    INSERT INTO #line_pricing (
        line_nbr, proc_cd, modifier_1, modifier_2, revenue_cd,
        dos_from, dos_to, pos_cd, charge_amt, units,
        bilateral_flag, assistant_flag
    )
    SELECT line_nbr, proc_cd, modifier_1, modifier_2, revenue_cd,
           date_of_service_from, date_of_service_to, place_of_service_cd,
           charge_amt, units_of_service,
           bilateral_flag, assistant_surgeon_flag
    FROM   dbo.CLAIM_DETAIL (NOLOCK)
    WHERE  claim_number = @claim_number
    AND    claim_suffix = @claim_suffix
    ORDER BY line_nbr

    SELECT @err_code = @@error, @row_cnt = @@rowcount
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 1010, @o_return_message = 'ERROR: Failed to load claim detail lines'
        GOTO ERROR_EXIT
    END
    IF @row_cnt = 0
    BEGIN
        SELECT @o_return_code = 1011, @o_return_message = 'ERROR: No claim detail lines found'
        GOTO ERROR_EXIT
    END

    SELECT @max_line = MAX(line_nbr) FROM #line_pricing

    /***************************************************************************
     * STEP 3: ELIGIBILITY VERIFICATION
     ***************************************************************************/
    SELECT @step_name = 'ELIG_CHECK'

    SELECT @elig_found          = 1,
           @elig_eff_dt         = e.elig_eff_dt,
           @elig_term_dt        = ISNULL(e.elig_term_dt, '2099-12-31'),
           @benefit_pkg_cd      = e.benefit_pkg_cd,
           @coverage_type_cd    = e.coverage_type_cd,
           @product_type_cd     = e.product_type_cd,
           @ind_deductible      = e.ind_deductible_amt,
           @fam_deductible      = e.fam_deductible_amt,
           @oon_ind_deductible  = e.oon_ind_deductible_amt,
           @oon_fam_deductible  = e.oon_fam_deductible_amt,
           @ind_oop_max         = e.ind_oop_max_amt,
           @fam_oop_max         = e.fam_oop_max_amt,
           @oon_ind_oop_max     = e.oon_ind_oop_max_amt,
           @oon_fam_oop_max     = e.oon_fam_oop_max_amt,
           @coinsurance_pct     = e.coinsurance_pct,
           @oon_coinsurance     = e.oon_coinsurance_pct,
           @er_copay            = e.er_copay_amt,
           @specialist_copay    = e.specialist_copay_amt,
           @pcp_copay           = e.pcp_copay_amt,
           @embedded_ded_flag   = e.embedded_deductible_flag,
           @embedded_oop_flag   = e.embedded_oop_flag,
           @plan_year_start     = e.plan_year_start_dt,
           @plan_year_end       = e.plan_year_end_dt,
           @lifetime_max        = e.lifetime_max_amt,
           @grandfathered_flag  = e.grandfathered_flag
    FROM   dbo.ELIGIBILITY e (NOLOCK)
    WHERE  e.member_id      = @member_id
    AND    e.member_suffix   = @member_suffix
    AND    e.payer_id        = @payer_id
    AND    e.plan_cd         = @plan_cd
    AND    e.active_flag     = 'Y'
    AND    e.elig_eff_dt    <= (SELECT MIN(dos_from) FROM #line_pricing)
    AND    ISNULL(e.elig_term_dt, '2099-12-31') >= (SELECT MAX(dos_to) FROM #line_pricing)

    SELECT @err_code = @@error, @row_cnt = @@rowcount
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 2001, @o_return_message = 'ERROR: Eligibility lookup failed'
        GOTO ERROR_EXIT
    END
    IF @row_cnt = 0
    BEGIN
        /* Member not eligible - pend for review */
        SELECT @o_return_code = 2002,
               @o_return_message = 'PEND: Member not eligible for dates of service'

        INSERT INTO dbo.PEND_QUEUE (claim_number, claim_suffix, pend_reason_cd,
            pend_reason_desc, pend_category_cd, pend_priority_cd, pend_due_dt, assigned_queue_cd)
        VALUES (@claim_number, @claim_suffix, 'ELIG',
            'Member not eligible for dates of service', 'ELIG', 'HIGH',
            DATEADD(day, 14, GETDATE()), 'ELIG_REV')

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 2003, @o_return_message = 'ERROR: Failed to insert pend queue'
            GOTO ERROR_EXIT
        END

        UPDATE dbo.CLAIM_HEADER
        SET    claim_status_cd = 'PEND', claim_sub_status_cd = 'ELIG',
               pend_reason_cd = 'ELIG', pend_dt = GETDATE(),
               modified_by = SUSER_NAME(), modified_dt = GETDATE()
        WHERE  claim_number = @claim_number AND claim_suffix = @claim_suffix

        SELECT @err_code = @@error
        IF @err_code != 0
            SELECT @o_return_code = 2004, @o_return_message = 'ERROR: Failed to update claim status to PEND'
        GOTO CLEAN_EXIT
    END

    /***************************************************************************
     * STEP 4: PROVIDER VALIDATION
     ***************************************************************************/
    SELECT @step_name = 'PROV_CHECK'

    SELECT @prov_specialty      = p.specialty_cd,
           @prov_locality       = p.locality_cd,
           @prov_gpci_locality  = p.gpci_locality_cd,
           @prov_sanction_flag  = p.sanction_flag,
           @prov_contract_id    = p.contract_id,
           @prov_withhold_pct   = ISNULL(p.withhold_pct, 0.0000),
           @prov_capitated      = p.capitated_flag,
           @wage_index_cbsa     = p.wage_index_cbsa,
           @teaching_hosp_flag  = p.teaching_hospital_flag,
           @dsh_pct             = ISNULL(p.dsh_pct, 0.0000),
           @ime_ratio           = ISNULL(p.ime_ratio, 0.0000)
    FROM   dbo.PROVIDER_MASTER p (NOLOCK)
    WHERE  p.provider_id = @rendering_prov_id

    SELECT @err_code = @@error, @row_cnt = @@rowcount
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 3001, @o_return_message = 'ERROR: Provider lookup failed'
        GOTO ERROR_EXIT
    END
    IF @row_cnt = 0
    BEGIN
        SELECT @o_return_code = 3002, @o_return_message = 'PEND: Rendering provider not found'
        GOTO ERROR_EXIT
    END

    /* Check provider sanctions */
    IF @prov_sanction_flag = 'Y'
    BEGIN
        INSERT INTO dbo.PEND_QUEUE (claim_number, claim_suffix, pend_reason_cd,
            pend_reason_desc, pend_category_cd, pend_priority_cd, pend_due_dt, assigned_queue_cd)
        VALUES (@claim_number, @claim_suffix, 'SANCTION',
            'Provider is sanctioned/excluded - manual review required', 'REVIEW', 'CRIT',
            DATEADD(day, 3, GETDATE()), 'COMPLIANCE')

        UPDATE dbo.CLAIM_HEADER
        SET    claim_status_cd = 'PEND', claim_sub_status_cd = 'SANCT',
               pend_reason_cd = 'SANCTION', pend_dt = GETDATE(),
               modified_by = SUSER_NAME(), modified_dt = GETDATE()
        WHERE  claim_number = @claim_number AND claim_suffix = @claim_suffix

        SELECT @o_return_code = 3004, @o_return_message = 'PEND: Provider sanctioned'
        GOTO CLEAN_EXIT
    END

    /***************************************************************************
     * STEP 5: BEGIN TRANSACTION - PRICING AND BENEFIT APPLICATION
     ***************************************************************************/
    SELECT @step_name = 'BEGIN_TRAN'
    BEGIN TRAN ADJUDICATE_CLAIM
    SELECT @tran_active = 1, @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 4001, @o_return_message = 'ERROR: Failed to begin transaction'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 6: PRICING - DETERMINE METHOD AND CALCULATE ALLOWED AMOUNTS
     ***************************************************************************/
    SELECT @step_name = 'PRICING'

    IF @claim_type_cd = 'INS' AND @drg_cd IS NOT NULL
    BEGIN
        /***************************************************************
         * DRG PRICING with base rate, wage index, DSH, IME, outlier
         ***************************************************************/
        SELECT @pricing_method = 'DRG'

        /* Get DRG weight (use SOI-adjusted if available) */
        SELECT @drg_adj_weight = CASE
                   WHEN @drg_soi_cd = '1' AND dw.soi_level_1_wt IS NOT NULL THEN dw.soi_level_1_wt
                   WHEN @drg_soi_cd = '2' AND dw.soi_level_2_wt IS NOT NULL THEN dw.soi_level_2_wt
                   WHEN @drg_soi_cd = '3' AND dw.soi_level_3_wt IS NOT NULL THEN dw.soi_level_3_wt
                   WHEN @drg_soi_cd = '4' AND dw.soi_level_4_wt IS NOT NULL THEN dw.soi_level_4_wt
                   ELSE dw.drg_weight
               END,
               @drg_outlier_thresh = dw.outlier_threshold_amt
        FROM   dbo.DRG_WEIGHT dw (NOLOCK)
        WHERE  dw.drg_cd = @drg_cd AND dw.drg_type_cd = 'MS' AND dw.active_flag = 'Y'
        AND    dw.eff_dt <= @admission_dt
        AND    ISNULL(dw.term_dt, '2099-12-31') >= @admission_dt

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 5001, @o_return_message = 'ERROR: DRG weight lookup failed'
            GOTO ERROR_EXIT
        END

        /* Get contract base rate or use default Medicare base rate */
        SELECT @drg_base_rate = ISNULL(cr.rate_amt, 7000.00)
        FROM   dbo.CONTRACT_RATES cr (NOLOCK)
        WHERE  cr.contract_id = @prov_contract_id AND cr.rate_type_cd = 'DRG' AND cr.payer_id = @payer_id
        AND    (cr.drg_cd = @drg_cd OR cr.drg_cd IS NULL) AND cr.active_flag = 'Y'
        AND    cr.eff_dt <= @admission_dt AND ISNULL(cr.term_dt, '2099-12-31') >= @admission_dt

        IF @drg_base_rate IS NULL SELECT @drg_base_rate = 7000.00

        /* Labor/non-labor split (national standard ~68.2% labor) */
        SELECT @drg_labor_share = 0.6820, @drg_nonlabor_share = 0.3180

        /* Get wage index for provider CBSA */
        SELECT @drg_wage_index = 1.0000
        IF @wage_index_cbsa IS NOT NULL
        BEGIN
            SELECT @drg_wage_index = ISNULL(
                (SELECT CONVERT(decimal(9,6), config_value) FROM dbo.PAYER_CONFIG (NOLOCK)
                 WHERE payer_id = @payer_id AND config_key = 'WAGE_INDEX_' + @wage_index_cbsa
                 AND active_flag = 'Y' AND eff_dt <= @admission_dt), 1.0000)
        END

        /* Calculate operating payment */
        SELECT @drg_operating_amt = @drg_base_rate *
               ((@drg_labor_share * @drg_wage_index) + @drg_nonlabor_share) * @drg_adj_weight

        /* DSH adjustment */
        SELECT @drg_dsh_payment = 0.00
        IF @dsh_pct > 0.0000
            SELECT @drg_dsh_payment = @drg_operating_amt * (@dsh_pct / 100.0)

        /* IME adjustment */
        SELECT @drg_ime_payment = 0.00
        IF @teaching_hosp_flag = 'Y' AND @ime_ratio > 0.0000
            SELECT @drg_ime_payment = @drg_operating_amt * (1.35 * (POWER((1.0 + @ime_ratio / 100.0), 0.405) - 1.0))

        /* Capital payment */
        SELECT @drg_capital_amt = 462.43 * @drg_adj_weight

        /* Outlier calculation */
        SELECT @drg_outlier_amt = 0.00, @drg_cost_to_chrg = 0.3500
        IF @total_charge_amt * @drg_cost_to_chrg >
           (@drg_operating_amt + @drg_dsh_payment + @drg_ime_payment + @drg_capital_amt + ISNULL(@drg_outlier_thresh, 32006.00))
        BEGIN
            SELECT @drg_outlier_amt = (@total_charge_amt * @drg_cost_to_chrg -
                   (@drg_operating_amt + @drg_dsh_payment + @drg_ime_payment +
                    @drg_capital_amt + ISNULL(@drg_outlier_thresh, 32006.00))) * 0.80
        END

        /* Total DRG payment */
        SELECT @drg_total_payment = @drg_operating_amt + @drg_dsh_payment +
               @drg_ime_payment + @drg_capital_amt + @drg_outlier_amt

        /* Apply to lines proportionally */
        UPDATE #line_pricing
        SET    allowed_amt = CASE
                   WHEN @total_charge_amt > 0
                   THEN ROUND((@drg_total_payment * charge_amt / @total_charge_amt), 2)
                   ELSE ROUND((@drg_total_payment / (SELECT COUNT(*) FROM #line_pricing)), 2)
               END,
               pricing_method = 'DRG'

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 5010, @o_return_message = 'ERROR: DRG pricing distribution failed'
            GOTO ERROR_EXIT
        END
    END
    ELSE IF @claim_type_cd = 'INS' AND @drg_cd IS NULL
    BEGIN
        /***************************************************************
         * PER DIEM PRICING with step-down and ICU differential
         ***************************************************************/
        SELECT @pricing_method = 'PERDIEM'

        SELECT @perdiem_day1_rate   = ISNULL(cr.rate_per_diem_day_1, 1500.00),
               @perdiem_day2_5      = ISNULL(cr.rate_per_diem_day_2_5, 1200.00),
               @perdiem_day6_plus   = ISNULL(cr.rate_per_diem_day_6_plus, 1000.00)
        FROM   dbo.CONTRACT_RATES cr (NOLOCK)
        WHERE  cr.contract_id = @prov_contract_id AND cr.rate_type_cd = 'PERDI' AND cr.payer_id = @payer_id
        AND    cr.active_flag = 'Y' AND cr.eff_dt <= @admission_dt
        AND    ISNULL(cr.term_dt, '2099-12-31') >= @admission_dt

        IF @perdiem_day1_rate IS NULL
            SELECT @perdiem_day1_rate = 1500.00, @perdiem_day2_5 = 1200.00, @perdiem_day6_plus = 1000.00

        SELECT @perdiem_day_count = ISNULL(@los_days, DATEDIFF(day, @admission_dt, ISNULL(@discharge_dt, GETDATE())))
        IF @perdiem_day_count < 1 SELECT @perdiem_day_count = 1

        /* Step-down calculation */
        SELECT @perdiem_total = @perdiem_day1_rate
        IF @perdiem_day_count > 1
            SELECT @perdiem_total = @perdiem_total +
                (CASE WHEN @perdiem_day_count - 1 > 4 THEN 4 ELSE @perdiem_day_count - 1 END) * @perdiem_day2_5
        IF @perdiem_day_count > 5
            SELECT @perdiem_total = @perdiem_total + (@perdiem_day_count - 5) * @perdiem_day6_plus

        /* ICU differential: check for ICU revenue codes (0200-0219) */
        IF EXISTS (SELECT 1 FROM #line_pricing WHERE revenue_cd BETWEEN '0200' AND '0219')
        BEGIN
            DECLARE @icu_days smallint
            SELECT @icu_days = ISNULL(SUM(units), 0) FROM #line_pricing WHERE revenue_cd BETWEEN '0200' AND '0219'
            SELECT @perdiem_total = @perdiem_total + @icu_days * 500.00
        END

        IF @perdiem_total > @total_charge_amt SELECT @perdiem_total = @total_charge_amt

        UPDATE #line_pricing
        SET    allowed_amt = CASE
                   WHEN @total_charge_amt > 0
                   THEN ROUND((@perdiem_total * charge_amt / @total_charge_amt), 2)
                   ELSE ROUND((@perdiem_total / (SELECT COUNT(*) FROM #line_pricing)), 2)
               END,
               pricing_method = 'PERDIEM'

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 5030, @o_return_message = 'ERROR: Per diem pricing distribution failed'
            GOTO ERROR_EXIT
        END
    END
    ELSE
    BEGIN
        /***************************************************************
         * PROFESSIONAL / OUTPATIENT LINE-LEVEL PRICING (RBRVS)
         ***************************************************************/
        SELECT @pricing_method = 'LINE'
        SELECT @current_line = 0

        WHILE @current_line < @max_line
        BEGIN
            SELECT @current_line = MIN(line_nbr) FROM #line_pricing WHERE line_nbr > @current_line
            IF @current_line IS NULL BREAK

            SELECT @line_proc_cd = proc_cd, @line_modifier_1 = modifier_1, @line_modifier_2 = modifier_2,
                   @line_charge_amt = charge_amt, @line_units = units, @line_dos_from = dos_from, @line_pos_cd = pos_cd
            FROM   #line_pricing WHERE line_nbr = @current_line

            /* Contract rate lookup first */
            SELECT @line_allowed_amt = NULL
            SELECT @line_allowed_amt = cr.rate_amt
            FROM   dbo.CONTRACT_RATES cr (NOLOCK)
            WHERE  cr.contract_id = @prov_contract_id AND cr.payer_id = @payer_id AND cr.proc_cd = @line_proc_cd
            AND    (cr.modifier_cd = @line_modifier_1 OR cr.modifier_cd IS NULL) AND cr.active_flag = 'Y'
            AND    cr.eff_dt <= @line_dos_from AND ISNULL(cr.term_dt, '2099-12-31') >= @line_dos_from

            SELECT @err_code = @@error
            IF @err_code != 0
            BEGIN
                SELECT @o_return_code = 5040, @o_return_message = 'ERROR: Contract rate lookup failed'
                GOTO ERROR_EXIT
            END

            IF @line_allowed_amt IS NOT NULL
            BEGIN
                UPDATE #line_pricing SET allowed_amt = @line_allowed_amt * units,
                       pricing_method = 'CONTRACT', fee_schedule_id = @prov_contract_id
                WHERE  line_nbr = @current_line
            END
            ELSE
            BEGIN
                /* RBRVS fee schedule lookup */
                SELECT @work_rvu = NULL, @pe_rvu = NULL, @mp_rvu = NULL
                SELECT @work_rvu = fs.work_rvu,
                       @pe_rvu   = CASE WHEN @line_pos_cd IN ('11','22','49','71','72')
                                        THEN fs.pe_rvu_non_facility ELSE fs.pe_rvu_facility END,
                       @mp_rvu   = fs.mp_rvu,
                       @work_gpci = fs.work_gpci, @pe_gpci = fs.pe_gpci, @mp_gpci = fs.mp_gpci,
                       @conversion_factor = fs.conversion_factor
                FROM   dbo.FEE_SCHEDULE fs (NOLOCK)
                WHERE  fs.fee_schedule_id = (SELECT ISNULL(
                           (SELECT TOP 1 config_value FROM dbo.PAYER_CONFIG (NOLOCK)
                            WHERE payer_id = @payer_id AND config_key = 'FEE_SCHEDULE_ID'
                            AND active_flag = 'Y' AND eff_dt <= @line_dos_from), 'MEDICARE'))
                AND    fs.proc_cd = @line_proc_cd AND fs.locality_cd = ISNULL(@prov_gpci_locality, '00')
                AND    fs.status_cd = 'A' AND fs.eff_dt <= @line_dos_from
                AND    ISNULL(fs.term_dt, '2099-12-31') >= @line_dos_from

                SELECT @err_code = @@error
                IF @err_code != 0
                BEGIN
                    SELECT @o_return_code = 5050, @o_return_message = 'ERROR: Fee schedule lookup failed'
                    GOTO ERROR_EXIT
                END

                IF @work_rvu IS NOT NULL
                BEGIN
                    /* RBRVS: (Work RVU * Work GPCI + PE RVU * PE GPCI + MP RVU * MP GPCI) * CF */
                    SELECT @rbrvs_allowed =
                        ((@work_rvu * ISNULL(@work_gpci, 1.0)) +
                         (@pe_rvu   * ISNULL(@pe_gpci, 1.0)) +
                         (@mp_rvu   * ISNULL(@mp_gpci, 1.0))) * ISNULL(@conversion_factor, 33.89)

                    /* Bilateral (mod 50) */
                    IF @line_modifier_1 = '50' OR @line_modifier_2 = '50'
                        SELECT @rbrvs_allowed = @rbrvs_allowed * 1.50

                    /* Assistant surgeon (mod 80/82) */
                    IF @line_modifier_1 IN ('80','82') OR @line_modifier_2 IN ('80','82')
                        SELECT @rbrvs_allowed = @rbrvs_allowed * 0.16

                    /* Multiple procedure rank */
                    DECLARE @mp_rank smallint
                    SELECT @mp_rank = multi_proc_rank FROM #line_pricing WHERE line_nbr = @current_line
                    IF @mp_rank IS NOT NULL AND @mp_rank >= 2
                        SELECT @rbrvs_allowed = @rbrvs_allowed * 0.50

                    UPDATE #line_pricing
                    SET    allowed_amt = ROUND(@rbrvs_allowed * units, 2),
                           pricing_method = 'RBRVS', work_rvu = @work_rvu, pe_rvu = @pe_rvu, mp_rvu = @mp_rvu
                    WHERE  line_nbr = @current_line
                END
                ELSE
                BEGIN
                    /* Fallback: percent of charges */
                    DECLARE @pct_of_charges decimal(7,4)
                    SELECT @pct_of_charges = ISNULL(
                        (SELECT CONVERT(decimal(7,4), config_value) FROM dbo.PAYER_CONFIG (NOLOCK)
                         WHERE payer_id = @payer_id AND config_key = 'DEFAULT_PCT_OF_CHARGES'
                         AND active_flag = 'Y'), 70.0000)

                    UPDATE #line_pricing SET allowed_amt = ROUND(charge_amt * @pct_of_charges / 100.0, 2),
                           pricing_method = 'PCTCHG'
                    WHERE  line_nbr = @current_line
                END
            END

            /* Lesser-of logic */
            UPDATE #line_pricing SET allowed_amt = charge_amt
            WHERE  line_nbr = @current_line AND allowed_amt > charge_amt

            SELECT @err_code = @@error
            IF @err_code != 0
            BEGIN
                SELECT @o_return_code = 5060, @o_return_message = 'ERROR: Lesser-of adjustment failed'
                GOTO ERROR_EXIT
            END
        END
    END

    /***************************************************************************
     * STEP 7: ACCUMULATOR LOAD (HOLDLOCK for concurrent reservation)
     ***************************************************************************/
    SELECT @step_name = 'ACCUM_LOAD'
    DECLARE @eff_tier varchar(3)
    SELECT @eff_tier = CASE WHEN @par_flag = 'Y' THEN 'INN' ELSE 'OON' END

    INSERT INTO #accum_work (accum_type_cd, network_tier, accum_level, limit_amt, applied_amt, reserved_amt, remaining_amt, accum_id)
    SELECT ba.accum_type_cd, ba.network_tier_cd, ba.accum_level_cd,
           ba.benefit_limit_amt, ba.accum_applied_amt, ba.accum_reserved_amt, ba.accum_remaining_amt, ba.accum_id
    FROM   dbo.BENEFIT_ACCUMULATORS ba (HOLDLOCK)
    WHERE  ba.member_id = @member_id AND ba.member_suffix = @member_suffix
    AND    ba.payer_id = @payer_id AND ba.plan_cd = @plan_cd
    AND    ba.plan_year_start_dt = @plan_year_start AND ba.network_tier_cd IN (@eff_tier, 'COMB')

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 6001, @o_return_message = 'ERROR: Accumulator load failed'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 8: DEDUCTIBLE APPLICATION
     ***************************************************************************/
    SELECT @step_name = 'DEDUCTIBLE'

    SELECT @deduct_remaining = ISNULL(remaining_amt, 0.00) FROM #accum_work
    WHERE  accum_type_cd = 'DEDUCT' AND network_tier = @eff_tier AND accum_level = 'IND'

    IF @deduct_remaining IS NULL
        SELECT @deduct_remaining = CASE WHEN @eff_tier = 'INN' THEN @ind_deductible ELSE @oon_ind_deductible END

    /* Embedded family deductible logic */
    IF @embedded_ded_flag = 'Y' AND @coverage_type_cd IN ('FAM', 'ESP', 'ECH', 'EF1', 'EF2')
    BEGIN
        INSERT INTO #family_accum (subscriber_id, member_id, member_suffix, ind_deduct_met)
        SELECT ba.subscriber_id, ba.member_id, ba.member_suffix, ba.accum_applied_amt + ba.accum_reserved_amt
        FROM   dbo.BENEFIT_ACCUMULATORS ba (NOLOCK)
        WHERE  ba.subscriber_id = @subscriber_id AND ba.payer_id = @payer_id AND ba.plan_cd = @plan_cd
        AND    ba.plan_year_start_dt = @plan_year_start AND ba.accum_type_cd = 'DEDUCT'
        AND    ba.network_tier_cd = @eff_tier AND ba.accum_level_cd = 'IND'

        UPDATE #family_accum SET ind_deduct_flag = 'Y'
        WHERE  ind_deduct_met >= CASE WHEN @eff_tier = 'INN' THEN @ind_deductible ELSE @oon_ind_deductible END

        SELECT @fam_mbr_ded_cnt = COUNT(*) FROM #family_accum WHERE ind_deduct_flag = 'Y'

        SELECT @fam_deduct_applied = ISNULL(
            (SELECT applied_amt + reserved_amt FROM #accum_work
             WHERE accum_type_cd = 'DEDUCT' AND network_tier = @eff_tier AND accum_level = 'FAM'), 0.00)

        SELECT @fam_deduct_met = CASE
            WHEN @fam_mbr_ded_cnt >= 2 THEN 'Y'
            WHEN @fam_deduct_applied >= CASE WHEN @eff_tier = 'INN' THEN @fam_deductible ELSE @oon_fam_deductible END THEN 'Y'
            ELSE 'N' END

        IF @fam_deduct_met = 'Y' SELECT @deduct_remaining = 0.00
    END

    DECLARE @total_allowed_for_ded decimal(13,2)
    SELECT @total_allowed_for_ded = SUM(allowed_amt) FROM #line_pricing

    SELECT @deduct_applied = CASE
        WHEN @deduct_remaining <= 0.00 THEN 0.00
        WHEN @deduct_remaining >= @total_allowed_for_ded THEN @total_allowed_for_ded
        ELSE @deduct_remaining END

    IF @deduct_applied > 0.00
    BEGIN
        UPDATE #line_pricing SET deductible_amt = CASE
            WHEN @total_allowed_for_ded > 0 THEN ROUND((@deduct_applied * allowed_amt / @total_allowed_for_ded), 2)
            ELSE 0.00 END

        /* Reserve deductible in accumulators with holdlock */
        UPDATE dbo.BENEFIT_ACCUMULATORS
        SET    accum_reserved_amt = accum_reserved_amt + @deduct_applied,
               accum_remaining_amt = accum_remaining_amt - @deduct_applied,
               concurrent_lock_id = @claim_number, concurrent_lock_dt = GETDATE(),
               last_claim_nbr = @claim_number, last_update_dt = GETDATE(),
               last_update_by = SUSER_NAME(), modified_dt = GETDATE(), row_version = row_version + 1
        WHERE  accum_id = (SELECT accum_id FROM #accum_work
                           WHERE accum_type_cd = 'DEDUCT' AND network_tier = @eff_tier AND accum_level = 'IND')

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 6021, @o_return_message = 'ERROR: Deductible reservation failed'
            GOTO ERROR_EXIT
        END
    END

    /***************************************************************************
     * STEP 9: COPAY APPLICATION BY SERVICE TYPE
     ***************************************************************************/
    SELECT @step_name = 'COPAY'

    UPDATE lp SET copay_amt = CASE
        WHEN lp.pos_cd = '23' OR lp.revenue_cd BETWEEN '0450' AND '0459' THEN @er_copay
        WHEN lp.pos_cd = '11' AND @prov_specialty NOT IN ('08','11','38') THEN @specialist_copay
        WHEN lp.pos_cd = '11' AND @prov_specialty IN ('08','11','38') THEN @pcp_copay
        ELSE 0.00 END
    FROM   #line_pricing lp
    WHERE  lp.line_nbr = (SELECT MIN(line_nbr) FROM #line_pricing)

    UPDATE #line_pricing SET copay_amt = CASE
        WHEN copay_amt > (allowed_amt - deductible_amt) THEN (allowed_amt - deductible_amt)
        WHEN copay_amt < 0 THEN 0.00 ELSE copay_amt END

    /***************************************************************************
     * STEP 10: COINSURANCE CALCULATION
     ***************************************************************************/
    SELECT @step_name = 'COINSURANCE'
    DECLARE @eff_coinsurance decimal(7,4)
    SELECT @eff_coinsurance = CASE WHEN @eff_tier = 'OON' THEN @oon_coinsurance ELSE @coinsurance_pct END

    UPDATE #line_pricing SET coinsurance_amt = ROUND(
        (allowed_amt - deductible_amt - copay_amt) * ((100.0 - @eff_coinsurance) / 100.0), 2)
    WHERE  (allowed_amt - deductible_amt - copay_amt) > 0

    UPDATE #line_pricing SET coinsurance_amt = 0.00 WHERE coinsurance_amt < 0.00

    /***************************************************************************
     * STEP 11: OOP MAXIMUM CHECK AND REDISTRIBUTION
     ***************************************************************************/
    SELECT @step_name = 'OOP_CHECK'

    SELECT @oop_applied = ISNULL(
        (SELECT applied_amt + reserved_amt FROM #accum_work
         WHERE accum_type_cd = 'OOP' AND network_tier = @eff_tier AND accum_level = 'IND'), 0.00)

    DECLARE @eff_oop_max decimal(13,2)
    SELECT @eff_oop_max = CASE WHEN @eff_tier = 'OON' THEN @oon_ind_oop_max ELSE @ind_oop_max END

    DECLARE @this_claim_patient_resp decimal(13,2)
    SELECT @this_claim_patient_resp = SUM(deductible_amt + copay_amt + coinsurance_amt) FROM #line_pricing

    SELECT @oop_remaining = @eff_oop_max - @oop_applied
    IF @oop_remaining < 0 SELECT @oop_remaining = 0.00

    IF @this_claim_patient_resp > @oop_remaining AND @eff_oop_max > 0
    BEGIN
        DECLARE @oop_excess decimal(13,2)
        SELECT @oop_excess = @this_claim_patient_resp - @oop_remaining

        /* Reduce coinsurance first */
        DECLARE @coins_total decimal(13,2)
        SELECT @coins_total = SUM(coinsurance_amt) FROM #line_pricing

        IF @oop_excess <= @coins_total
        BEGIN
            UPDATE #line_pricing SET coinsurance_amt = CASE
                WHEN @coins_total > 0 THEN ROUND(coinsurance_amt * (1.0 - @oop_excess / @coins_total), 2)
                ELSE 0.00 END
        END
        ELSE
        BEGIN
            UPDATE #line_pricing SET coinsurance_amt = 0.00
            DECLARE @remaining_excess decimal(13,2)
            SELECT @remaining_excess = @oop_excess - @coins_total

            DECLARE @copay_total decimal(13,2)
            SELECT @copay_total = SUM(copay_amt) FROM #line_pricing

            IF @remaining_excess <= @copay_total
                UPDATE #line_pricing SET copay_amt = CASE
                    WHEN @copay_total > 0 THEN ROUND(copay_amt * (1.0 - @remaining_excess / @copay_total), 2)
                    ELSE 0.00 END
            ELSE
            BEGIN
                UPDATE #line_pricing SET copay_amt = 0.00
                SELECT @remaining_excess = @remaining_excess - @copay_total
                UPDATE #line_pricing SET deductible_amt = CASE
                    WHEN deductible_amt > 0 AND @deduct_applied > 0
                    THEN ROUND(deductible_amt * (1.0 - @remaining_excess / @deduct_applied), 2) ELSE 0.00 END
            END
        END

        UPDATE dbo.BENEFIT_ACCUMULATORS
        SET    accum_reserved_amt = accum_reserved_amt + @oop_remaining, accum_remaining_amt = 0.00,
               last_claim_nbr = @claim_number, last_update_dt = GETDATE(), modified_dt = GETDATE(), row_version = row_version + 1
        WHERE  accum_id = (SELECT accum_id FROM #accum_work WHERE accum_type_cd = 'OOP' AND network_tier = @eff_tier AND accum_level = 'IND')
    END
    ELSE IF @eff_oop_max > 0
    BEGIN
        UPDATE dbo.BENEFIT_ACCUMULATORS
        SET    accum_reserved_amt = accum_reserved_amt + @this_claim_patient_resp,
               accum_remaining_amt = accum_remaining_amt - @this_claim_patient_resp,
               last_claim_nbr = @claim_number, last_update_dt = GETDATE(), modified_dt = GETDATE(), row_version = row_version + 1
        WHERE  accum_id = (SELECT accum_id FROM #accum_work WHERE accum_type_cd = 'OOP' AND network_tier = @eff_tier AND accum_level = 'IND')
    END

    /***************************************************************************
     * STEP 12: COB SECONDARY PAYMENT CALCULATION
     ***************************************************************************/
    SELECT @step_name = 'COB'

    IF @cob_cd IN ('S', 'T')
    BEGIN
        SELECT @cob_other_paid = ISNULL(ch.other_payer_paid_amt, 0.00)
        FROM   dbo.COB_HISTORY ch (NOLOCK)
        WHERE  ch.claim_number = @claim_number AND ch.claim_suffix = @claim_suffix AND ch.cob_order_cd = 'P'

        SELECT @cob_method_cd = ISNULL(
            (SELECT config_value FROM dbo.PAYER_CONFIG (NOLOCK)
             WHERE payer_id = @payer_id AND config_key = 'COB_METHOD' AND active_flag = 'Y'), 'STD')

        DECLARE @this_plan_normal decimal(13,2)
        SELECT @this_plan_normal = SUM(allowed_amt - deductible_amt - copay_amt - coinsurance_amt) FROM #line_pricing

        DECLARE @total_allowed_cob decimal(13,2)
        SELECT @total_allowed_cob = SUM(allowed_amt) FROM #line_pricing

        IF @cob_method_cd = 'STD'
        BEGIN
            SELECT @cob_secondary_pay = CASE
                WHEN @this_plan_normal <= (@total_allowed_cob - @cob_other_paid) THEN @this_plan_normal
                ELSE @total_allowed_cob - @cob_other_paid END
            IF @cob_secondary_pay < 0 SELECT @cob_secondary_pay = 0.00
        END
        ELSE
        BEGIN
            SELECT @cob_secondary_pay = @total_allowed_cob - @cob_other_paid
            IF @cob_secondary_pay < 0 SELECT @cob_secondary_pay = 0.00
            IF @cob_secondary_pay > @this_plan_normal SELECT @cob_secondary_pay = @this_plan_normal
        END

        SELECT @cob_savings = @this_plan_normal - @cob_secondary_pay
        IF @cob_savings < 0 SELECT @cob_savings = 0.00

        IF @cob_secondary_pay < @this_plan_normal
        BEGIN
            DECLARE @cob_reduction decimal(13,2)
            SELECT @cob_reduction = @this_plan_normal - @cob_secondary_pay
            UPDATE #line_pricing SET cob_amt = CASE
                WHEN @this_plan_normal > 0
                THEN ROUND(@cob_reduction * (allowed_amt - deductible_amt - copay_amt - coinsurance_amt) / @this_plan_normal, 2)
                ELSE 0.00 END
        END

        INSERT INTO dbo.COB_HISTORY (claim_number, claim_suffix, member_id, cob_order_cd,
            other_payer_id, other_payer_paid_amt, cob_method_cd, cob_savings_amt,
            this_plan_allowed_amt, this_plan_paid_amt, determination_dt, determination_method_cd)
        VALUES (@claim_number, @claim_suffix, @member_id, @cob_cd,
            ISNULL((SELECT other_payer_id FROM dbo.COB_HISTORY (NOLOCK) WHERE claim_number = @claim_number AND cob_order_cd = 'P'), 'UNKNOWN'),
            @cob_other_paid, @cob_method_cd, @cob_savings, @total_allowed_cob, @cob_secondary_pay, GETDATE(), 'AUTO')
    END

    /***************************************************************************
     * STEP 13: PROVIDER WITHHOLD
     ***************************************************************************/
    SELECT @step_name = 'WITHHOLD'

    IF @prov_withhold_pct > 0.0000 AND @par_flag = 'Y'
    BEGIN
        UPDATE #line_pricing SET withhold_amt = ROUND(
            (allowed_amt - deductible_amt - copay_amt - coinsurance_amt - cob_amt) * (@prov_withhold_pct / 100.0), 2)
        WHERE  (allowed_amt - deductible_amt - copay_amt - coinsurance_amt - cob_amt) > 0
    END

    /***************************************************************************
     * STEP 14: CALCULATE PAID AMOUNT AND PATIENT RESPONSIBILITY
     ***************************************************************************/
    SELECT @step_name = 'CALC_PAID'

    UPDATE #line_pricing
    SET    paid_amt = ROUND(allowed_amt - deductible_amt - copay_amt - coinsurance_amt - cob_amt - withhold_amt, 2),
           patient_resp_amt = ROUND(deductible_amt + copay_amt + coinsurance_amt + (charge_amt - allowed_amt), 2)

    UPDATE #line_pricing SET paid_amt = 0.00 WHERE paid_amt < 0.00
    UPDATE #line_pricing SET patient_resp_amt = 0.00 WHERE patient_resp_amt < 0.00

    /***************************************************************************
     * STEP 15: PROMPT PAY INTEREST CALCULATION
     ***************************************************************************/
    SELECT @step_name = 'INTEREST'

    SELECT @prompt_pay_days = ISNULL((SELECT pm.prompt_pay_days FROM dbo.PAYER_MASTER pm (NOLOCK) WHERE pm.payer_id = @payer_id), 30)
    SELECT @prompt_pay_pct = ISNULL((SELECT pm.prompt_pay_interest_pct FROM dbo.PAYER_MASTER pm (NOLOCK) WHERE pm.payer_id = @payer_id), 0.0000)
    SELECT @calendar_days = DATEDIFF(day, @receipt_dt, GETDATE())

    IF @calendar_days > @prompt_pay_days AND @prompt_pay_pct > 0.0000
    BEGIN
        DECLARE @overdue_days int
        SELECT @overdue_days = @calendar_days - @prompt_pay_days

        UPDATE #line_pricing SET interest_amt = ROUND(paid_amt * (@prompt_pay_pct / 100.0 / 365.0) * @overdue_days, 2)
        WHERE  paid_amt > 0
    END

    /***************************************************************************
     * STEP 16: UPDATE CLAIM DETAIL LINES
     ***************************************************************************/
    SELECT @step_name = 'UPDATE_LINES'

    UPDATE cd
    SET    cd.allowed_amt = lp.allowed_amt, cd.deductible_amt = lp.deductible_amt,
           cd.copay_amt = lp.copay_amt, cd.coinsurance_amt = lp.coinsurance_amt,
           cd.cob_amt = lp.cob_amt, cd.withhold_amt = lp.withhold_amt,
           cd.interest_amt = lp.interest_amt, cd.paid_amt = lp.paid_amt,
           cd.patient_resp_amt = lp.patient_resp_amt, cd.pricing_method_cd = lp.pricing_method,
           cd.fee_schedule_id = lp.fee_schedule_id, cd.work_rvu = lp.work_rvu,
           cd.pe_rvu = lp.pe_rvu, cd.mp_rvu = lp.mp_rvu,
           cd.line_status_cd = CASE WHEN lp.paid_amt > 0 THEN 'PAID' WHEN lp.denial_reason IS NOT NULL THEN 'DENY' ELSE 'APPR' END,
           cd.denial_reason_cd = lp.denial_reason, cd.carc_cd = lp.carc_cd, cd.rarc_cd = lp.rarc_cd,
           cd.modified_by = SUSER_NAME(), cd.modified_dt = GETDATE()
    FROM   dbo.CLAIM_DETAIL cd JOIN #line_pricing lp ON lp.line_nbr = cd.line_nbr
    WHERE  cd.claim_number = @claim_number AND cd.claim_suffix = @claim_suffix

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9060, @o_return_message = 'ERROR: Claim detail update failed'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 17: UPDATE HEADER TOTALS
     ***************************************************************************/
    SELECT @step_name = 'UPDATE_HEADER'

    SELECT @o_total_allowed_amt = ISNULL(SUM(allowed_amt), 0.00),
           @o_total_deduct_amt  = ISNULL(SUM(deductible_amt), 0.00),
           @o_total_copay_amt   = ISNULL(SUM(copay_amt), 0.00),
           @o_total_coins_amt   = ISNULL(SUM(coinsurance_amt), 0.00),
           @o_total_cob_amt     = ISNULL(SUM(cob_amt), 0.00),
           @o_total_withhold    = ISNULL(SUM(withhold_amt), 0.00),
           @o_total_interest    = ISNULL(SUM(interest_amt), 0.00),
           @o_total_paid_amt    = ISNULL(SUM(paid_amt + interest_amt), 0.00)
    FROM   #line_pricing

    UPDATE dbo.CLAIM_HEADER
    SET    claim_status_cd = CASE WHEN @o_total_paid_amt > 0 THEN 'APPR' ELSE 'DENY' END,
           claim_sub_status_cd = NULL, adjudication_dt = GETDATE(),
           total_allowed_amt = @o_total_allowed_amt, total_deductible_amt = @o_total_deduct_amt,
           total_copay_amt = @o_total_copay_amt, total_coinsurance_amt = @o_total_coins_amt,
           total_cob_amt = @o_total_cob_amt, total_withhold_amt = @o_total_withhold,
           total_interest_amt = @o_total_interest, total_paid_amt = @o_total_paid_amt,
           total_patient_resp_amt = ISNULL((SELECT SUM(patient_resp_amt) FROM #line_pricing), 0.00),
           total_noncovered_amt = ISNULL((SELECT SUM(charge_amt - allowed_amt) FROM #line_pricing WHERE charge_amt > allowed_amt), 0.00),
           pricing_method_cd = @pricing_method, pend_reason_cd = NULL, pend_dt = NULL,
           modified_by = SUSER_NAME(), modified_dt = GETDATE(), row_version = row_version + 1
    WHERE  claim_number = @claim_number AND claim_suffix = @claim_suffix

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9070, @o_return_message = 'ERROR: Claim header update failed'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 18: INSERT AUDIT TRAIL
     ***************************************************************************/
    SELECT @step_name = 'AUDIT'

    INSERT INTO dbo.CLAIM_AUDIT_LOG (claim_number, claim_suffix, audit_action_cd, audit_source_cd,
        table_name, audit_detail, status_before, status_after, amt_before, amt_after, user_id, program_id)
    VALUES (@claim_number, @claim_suffix, 'PAYMENT', 'SYSTEM', 'CLAIM_HEADER',
        'Adjudicated: Allowed=' + CONVERT(varchar, @o_total_allowed_amt) +
        ' Paid=' + CONVERT(varchar, @o_total_paid_amt) +
        ' Deduct=' + CONVERT(varchar, @o_total_deduct_amt) +
        ' Method=' + ISNULL(@pricing_method, 'N/A'),
        @claim_status_cd, CASE WHEN @o_total_paid_amt > 0 THEN 'APPR' ELSE 'DENY' END,
        0.00, @o_total_paid_amt, SUSER_NAME(), 'SP_ADJUDICATE_CLAIM')

    /***************************************************************************
     * STEP 19: COMMIT TRANSACTION
     ***************************************************************************/
    SELECT @step_name = 'COMMIT'
    COMMIT TRAN ADJUDICATE_CLAIM
    SELECT @tran_active = 0

    SELECT @o_return_code = 0,
           @o_return_message = 'SUCCESS: Claim ' + @claim_number + '/' + @claim_suffix +
                               ' adjudicated. Paid=' + CONVERT(varchar, @o_total_paid_amt)
    GOTO CLEAN_EXIT

    /***************************************************************************
     * ERROR EXIT
     ***************************************************************************/
    ERROR_EXIT:
        IF @tran_active = 1
        BEGIN
            ROLLBACK TRAN ADJUDICATE_CLAIM
            SELECT @tran_active = 0
        END
        INSERT INTO dbo.CLAIM_AUDIT_LOG (claim_number, claim_suffix, audit_action_cd, audit_source_cd,
            table_name, audit_detail, user_id, program_id)
        VALUES (@claim_number, @claim_suffix, 'STATUS', 'SYSTEM', 'CLAIM_HEADER',
            'Adjudication error at step ' + @step_name + ': ' + @o_return_message,
            SUSER_NAME(), 'SP_ADJUDICATE_CLAIM')

    /***************************************************************************
     * CLEAN EXIT
     ***************************************************************************/
    CLEAN_EXIT:
        IF OBJECT_ID('tempdb..#line_pricing') IS NOT NULL DROP TABLE #line_pricing
        IF OBJECT_ID('tempdb..#accum_work') IS NOT NULL DROP TABLE #accum_work
        IF OBJECT_ID('tempdb..#family_accum') IS NOT NULL DROP TABLE #family_accum

    RETURN @o_return_code
END
go

PRINT 'Stored procedure SP_ADJUDICATE_CLAIM created successfully.'
go

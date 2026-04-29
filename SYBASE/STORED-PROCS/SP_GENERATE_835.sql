/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Stored Procedure: SP_GENERATE_835
 *
 * Database:    HCPS_PROD
 * Version:     4.2.0
 * Created:     2024-04-01
 * Modified:    2026-04-20
 * Author:      HCPS EDI Team
 *
 * Description: Generates 835 Health Care Payment/Remittance Advice data
 *              into staging tables. Bundles claims by provider pay-to address,
 *              calculates check/EFT totals, assigns control numbers, handles
 *              adjustments/voids, and provider-level balance adjustments (PLB).
 *
 * Parameters (INPUT):
 *   @payment_run_id     - Payment run identifier
 *   @payer_id           - Payer (optional - NULL=all payers in run)
 *   @payee_id           - Specific payee (optional - NULL=all)
 *   @test_mode          - Y=generate but don't mark as sent, N=production
 *
 * Parameters (OUTPUT):
 *   @o_check_count      - Number of checks/EFTs generated
 *   @o_claim_count      - Number of claims included
 *   @o_total_paid_amt   - Total payment amount
 *   @o_file_count       - Number of 835 files generated
 *   @o_return_code      - 0=success, >0=error
 *   @o_return_message   - Result description
 ******************************************************************************/

USE HCPS_PROD
go

IF OBJECT_ID('dbo.SP_GENERATE_835') IS NOT NULL
    DROP PROCEDURE dbo.SP_GENERATE_835
go

CREATE PROCEDURE dbo.SP_GENERATE_835
    @payment_run_id     varchar(15),
    @payer_id           varchar(15)     = NULL,
    @payee_id           varchar(15)     = NULL,
    @test_mode          char(1)         = 'N',
    @o_check_count      int             OUTPUT,
    @o_claim_count      int             OUTPUT,
    @o_total_paid_amt   decimal(13,2)   OUTPUT,
    @o_file_count       int             OUTPUT,
    @o_return_code      int             OUTPUT,
    @o_return_message   varchar(255)    OUTPUT
AS
BEGIN
    SET NOCOUNT ON

    /***************************************************************************
     * LOCAL VARIABLE DECLARATIONS
     ***************************************************************************/
    DECLARE @err_code           int
    DECLARE @row_cnt            int
    DECLARE @step_name          varchar(50)
    DECLARE @tran_active        int

    /* Control numbers */
    DECLARE @isa_control_nbr    varchar(9)
    DECLARE @gs_control_nbr     varchar(9)
    DECLARE @st_control_nbr     varchar(9)
    DECLARE @isa_counter        numeric(15,0)
    DECLARE @gs_counter         numeric(15,0)
    DECLARE @st_counter         numeric(15,0)

    /* Payer info */
    DECLARE @payer_name         varchar(100)
    DECLARE @payer_tax_id       varchar(11)
    DECLARE @edi_sender_id      varchar(15)
    DECLARE @edi_receiver_id    varchar(15)
    DECLARE @payment_method_cd  varchar(5)

    /* Working variables */
    DECLARE @curr_payee_id      varchar(15)
    DECLARE @curr_check_nbr     varchar(20)
    DECLARE @curr_claim_nbr     varchar(15)
    DECLARE @curr_claim_suffix  varchar(2)
    DECLARE @payee_name         varchar(100)
    DECLARE @payee_npi          char(10)
    DECLARE @payee_tax_id       varchar(11)
    DECLARE @check_gross_amt    decimal(13,2)
    DECLARE @check_net_amt      decimal(13,2)
    DECLARE @check_withhold     decimal(13,2)
    DECLARE @check_plb_amt      decimal(13,2)
    DECLARE @check_claim_count  int
    DECLARE @trn_reference      varchar(30)

    /* PLB variables */
    DECLARE @plb_provider_id    varchar(15)
    DECLARE @plb_reason_cd      varchar(5)
    DECLARE @plb_amount         decimal(13,2)

    /* File tracking */
    DECLARE @file_name          varchar(100)
    DECLARE @generation_dt      datetime

    /***************************************************************************
     * INITIALIZATION
     ***************************************************************************/
    SELECT @o_check_count    = 0,
           @o_claim_count    = 0,
           @o_total_paid_amt = 0.00,
           @o_file_count     = 0,
           @o_return_code    = 0,
           @o_return_message = 'SUCCESS',
           @step_name        = 'INIT',
           @tran_active      = 0,
           @generation_dt    = GETDATE()

    /* Validate input */
    IF @payment_run_id IS NULL
    BEGIN
        SELECT @o_return_code = 1001, @o_return_message = 'ERROR: payment_run_id is required'
        RETURN @o_return_code
    END

    /***************************************************************************
     * CREATE TEMP TABLES
     ***************************************************************************/
    CREATE TABLE #payee_bundles (
        bundle_id           int IDENTITY    NOT NULL,
        payee_id            varchar(15)     NOT NULL,
        payee_name          varchar(100)    NULL,
        payee_npi           char(10)        NULL,
        payee_tax_id        varchar(11)     NULL,
        payee_address_1     varchar(55)     NULL,
        payee_city          varchar(30)     NULL,
        payee_state_cd      char(2)         NULL,
        payee_zip_cd        varchar(10)     NULL,
        check_eft_nbr       varchar(20)     NULL,
        payment_method_cd   varchar(5)      NOT NULL DEFAULT 'CHECK',
        gross_amt           decimal(13,2)   NOT NULL DEFAULT 0.00,
        withhold_amt        decimal(13,2)   NOT NULL DEFAULT 0.00,
        plb_adj_amt         decimal(13,2)   NOT NULL DEFAULT 0.00,
        net_amt             decimal(13,2)   NOT NULL DEFAULT 0.00,
        claim_count         int             NOT NULL DEFAULT 0,
        line_count          int             NOT NULL DEFAULT 0,
        payer_id            varchar(15)     NOT NULL
    )

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9001, @o_return_message = 'ERROR: Failed to create temp tables'
        RETURN @o_return_code
    END

    CREATE TABLE #plb_adjustments (
        plb_id              int IDENTITY    NOT NULL,
        payee_id            varchar(15)     NOT NULL,
        plb_reason_cd       varchar(5)      NOT NULL,
        plb_amount          decimal(13,2)   NOT NULL,
        plb_reference       varchar(30)     NULL,
        plb_period_start    datetime        NULL,
        plb_period_end      datetime        NULL
    )

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9002, @o_return_message = 'ERROR: Failed to create PLB temp table'
        GOTO CLEAN_EXIT
    END

    /***************************************************************************
     * STEP 1: VERIFY PAYMENT RUN EXISTS AND HAS UNPAID CLAIMS
     ***************************************************************************/
    SELECT @step_name = 'VERIFY_RUN'

    DECLARE @run_claim_count int
    SELECT @run_claim_count = COUNT(*)
    FROM   dbo.PAYMENT_HISTORY ph (NOLOCK)
    WHERE  ph.payment_run_id = @payment_run_id
    AND    ph.edi_835_sent_flag = 'N'
    AND    (@payer_id IS NULL OR EXISTS (
               SELECT 1 FROM dbo.CLAIM_HEADER ch (NOLOCK)
               WHERE ch.claim_number = ph.claim_number
               AND ch.payer_id = @payer_id))
    AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 2001, @o_return_message = 'ERROR: Payment run verification failed'
        GOTO CLEAN_EXIT
    END
    IF @run_claim_count = 0
    BEGIN
        SELECT @o_return_code = 2002, @o_return_message = 'No unsent payments found for run ' + @payment_run_id
        GOTO CLEAN_EXIT
    END

    /***************************************************************************
     * STEP 2: GET CONTROL NUMBERS (with lock for sequence)
     ***************************************************************************/
    SELECT @step_name = 'GET_CONTROL'

    BEGIN TRAN GEN_835
    SELECT @tran_active = 1

    /* ISA control number */
    UPDATE dbo.SYSTEM_SEQUENCE
    SET    current_value = current_value + 1,
           last_used_dt = GETDATE()
    WHERE  sequence_name = 'ISA_CONTROL'

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 3001, @o_return_message = 'ERROR: ISA control sequence update failed'
        GOTO ERROR_EXIT
    END

    SELECT @isa_counter = current_value FROM dbo.SYSTEM_SEQUENCE WHERE sequence_name = 'ISA_CONTROL'
    SELECT @isa_control_nbr = RIGHT('000000000' + CONVERT(varchar, @isa_counter), 9)

    /* GS control number */
    UPDATE dbo.SYSTEM_SEQUENCE
    SET    current_value = current_value + 1,
           last_used_dt = GETDATE()
    WHERE  sequence_name = 'GS_CONTROL'

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 3002, @o_return_message = 'ERROR: GS control sequence update failed'
        GOTO ERROR_EXIT
    END

    SELECT @gs_counter = current_value FROM dbo.SYSTEM_SEQUENCE WHERE sequence_name = 'GS_CONTROL'
    SELECT @gs_control_nbr = RIGHT('000000000' + CONVERT(varchar, @gs_counter), 9)

    /* ST control number */
    UPDATE dbo.SYSTEM_SEQUENCE
    SET    current_value = current_value + 1,
           last_used_dt = GETDATE()
    WHERE  sequence_name = 'ST_CONTROL'

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 3003, @o_return_message = 'ERROR: ST control sequence update failed'
        GOTO ERROR_EXIT
    END

    SELECT @st_counter = current_value FROM dbo.SYSTEM_SEQUENCE WHERE sequence_name = 'ST_CONTROL'
    SELECT @st_control_nbr = RIGHT('000000000' + CONVERT(varchar, @st_counter), 9)

    /***************************************************************************
     * STEP 3: GET PAYER INFORMATION
     ***************************************************************************/
    SELECT @step_name = 'GET_PAYER'

    IF @payer_id IS NOT NULL
    BEGIN
        SELECT @payer_name      = pm.payer_name,
               @payer_tax_id    = pm.tax_id,
               @edi_sender_id   = ISNULL(pm.edi_payer_id, pm.payer_id),
               @edi_receiver_id = pm.edi_receiver_id,
               @payment_method_cd = pm.payment_method_cd
        FROM   dbo.PAYER_MASTER pm (NOLOCK)
        WHERE  pm.payer_id = @payer_id

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 3010, @o_return_message = 'ERROR: Payer lookup failed'
            GOTO ERROR_EXIT
        END
    END
    ELSE
    BEGIN
        /* Use first payer in the run */
        SELECT @payer_id = ch.payer_id
        FROM   dbo.PAYMENT_HISTORY ph (NOLOCK)
        JOIN   dbo.CLAIM_HEADER ch (NOLOCK) ON ch.claim_number = ph.claim_number
        WHERE  ph.payment_run_id = @payment_run_id
        AND    ph.edi_835_sent_flag = 'N'

        SELECT @payer_name = pm.payer_name, @payer_tax_id = pm.tax_id,
               @edi_sender_id = ISNULL(pm.edi_payer_id, pm.payer_id),
               @edi_receiver_id = pm.edi_receiver_id,
               @payment_method_cd = pm.payment_method_cd
        FROM   dbo.PAYER_MASTER pm (NOLOCK) WHERE pm.payer_id = @payer_id
    END

    /***************************************************************************
     * STEP 4: BUILD PAYEE BUNDLES (Group claims by provider pay-to address)
     ***************************************************************************/
    SELECT @step_name = 'BUNDLE_PAYEES'

    INSERT INTO #payee_bundles (
        payee_id, payee_name, payee_npi, payee_tax_id,
        payee_address_1, payee_city, payee_state_cd, payee_zip_cd,
        payment_method_cd, gross_amt, withhold_amt, net_amt,
        claim_count, line_count, payer_id
    )
    SELECT ph.payee_id,
           MAX(ph.payee_name),
           MAX(ph.payee_npi),
           MAX(ph.payee_tax_id),
           MAX(pv.pay_to_address_1),
           MAX(pv.pay_to_city),
           MAX(pv.pay_to_state_cd),
           MAX(pv.pay_to_zip_cd),
           ISNULL(MAX(pm2.payment_method_cd), @payment_method_cd),
           SUM(ph.paid_amt + ph.interest_amt),
           SUM(ph.withhold_amt),
           SUM(ph.paid_amt + ph.interest_amt - ph.withhold_amt),
           COUNT(DISTINCT ph.claim_number + ph.claim_suffix),
           COUNT(*),
           @payer_id
    FROM   dbo.PAYMENT_HISTORY ph (NOLOCK)
    LEFT JOIN dbo.PROVIDER_MASTER pv (NOLOCK) ON pv.provider_id = ph.payee_id
    LEFT JOIN dbo.PAYER_MASTER pm2 (NOLOCK) ON pm2.payer_id = @payer_id
    WHERE  ph.payment_run_id = @payment_run_id
    AND    ph.edi_835_sent_flag = 'N'
    AND    ph.payment_type_cd IN ('PMT','ADJ','VOID','INT')
    AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)
    GROUP BY ph.payee_id

    SELECT @err_code = @@error, @row_cnt = @@rowcount
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 4001, @o_return_message = 'ERROR: Payee bundling failed'
        GOTO ERROR_EXIT
    END
    IF @row_cnt = 0
    BEGIN
        SELECT @o_return_code = 4002, @o_return_message = 'No payee bundles created'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 5: ASSIGN CHECK/EFT NUMBERS TO BUNDLES
     ***************************************************************************/
    SELECT @step_name = 'ASSIGN_CHECKS'

    /* Assign check numbers from existing CHECK_REGISTER or create new */
    DECLARE @bundle_id int, @max_bundle int
    SELECT @bundle_id = 0, @max_bundle = MAX(bundle_id) FROM #payee_bundles

    WHILE @bundle_id < @max_bundle
    BEGIN
        SELECT @bundle_id = MIN(bundle_id)
        FROM   #payee_bundles
        WHERE  bundle_id > @bundle_id

        IF @bundle_id IS NULL BREAK

        /* Look up existing check/EFT from CHECK_REGISTER */
        SELECT @curr_check_nbr = cr.check_eft_nbr
        FROM   dbo.CHECK_REGISTER cr (NOLOCK)
        WHERE  cr.payment_run_id = @payment_run_id
        AND    cr.payee_id = (SELECT payee_id FROM #payee_bundles WHERE bundle_id = @bundle_id)
        AND    cr.status_cd = 'ISSUE'

        SELECT @err_code = @@error
        IF @err_code != 0 OR @curr_check_nbr IS NULL
        BEGIN
            /* Generate new check number from sequence */
            UPDATE dbo.SYSTEM_SEQUENCE
            SET    current_value = current_value + 1, last_used_dt = GETDATE()
            WHERE  sequence_name = 'CHECK_NUMBER'

            SELECT @err_code = @@error
            IF @err_code != 0
            BEGIN
                SELECT @o_return_code = 5001, @o_return_message = 'ERROR: Check number generation failed'
                GOTO ERROR_EXIT
            END

            SELECT @curr_check_nbr = CONVERT(varchar, current_value)
            FROM   dbo.SYSTEM_SEQUENCE WHERE sequence_name = 'CHECK_NUMBER'
        END

        UPDATE #payee_bundles
        SET    check_eft_nbr = @curr_check_nbr
        WHERE  bundle_id = @bundle_id

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 5002, @o_return_message = 'ERROR: Check assignment failed'
            GOTO ERROR_EXIT
        END
    END

    /***************************************************************************
     * STEP 6: LOAD PLB (Provider-Level Balance) ADJUSTMENTS
     ***************************************************************************/
    SELECT @step_name = 'LOAD_PLB'

    INSERT INTO #plb_adjustments (payee_id, plb_reason_cd, plb_amount, plb_reference)
    SELECT ph.payee_id,
           CASE ph.payment_type_cd
               WHEN 'WHOLD' THEN 'WO'    /* Withhold */
               WHEN 'RECOV' THEN 'RE'    /* Recovery */
               WHEN 'REFUND' THEN 'AH'   /* Refund */
               WHEN 'PLB'   THEN 'CS'    /* Capitation settlement */
               ELSE 'AH'
           END,
           SUM(ph.paid_amt),
           @payment_run_id
    FROM   dbo.PAYMENT_HISTORY ph (NOLOCK)
    WHERE  ph.payment_run_id = @payment_run_id
    AND    ph.payment_type_cd IN ('WHOLD','RECOV','REFUND','PLB')
    AND    ph.edi_835_sent_flag = 'N'
    AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)
    GROUP BY ph.payee_id, ph.payment_type_cd

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        /* PLB failures are non-critical - continue */
        SELECT @o_return_message = @o_return_message + ' (WARNING: PLB load had errors)'
    END

    /* Update bundle PLB amounts */
    UPDATE pb
    SET    pb.plb_adj_amt = ISNULL(
               (SELECT SUM(pa.plb_amount) FROM #plb_adjustments pa
                WHERE pa.payee_id = pb.payee_id), 0.00),
           pb.net_amt = pb.gross_amt - pb.withhold_amt + ISNULL(
               (SELECT SUM(pa.plb_amount) FROM #plb_adjustments pa
                WHERE pa.payee_id = pb.payee_id), 0.00)
    FROM   #payee_bundles pb

    SELECT @err_code = @@error  /* Non-critical */

    /***************************************************************************
     * STEP 7: GENERATE REMITTANCE STAGING RECORDS
     ***************************************************************************/
    SELECT @step_name = 'GEN_STAGING'

    /* Clear any existing staging records for this run */
    DELETE FROM dbo.REMITTANCE_STAGING
    WHERE  payment_run_id = @payment_run_id
    AND    generated_flag = 'N'

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 7001, @o_return_message = 'ERROR: Staging cleanup failed'
        GOTO ERROR_EXIT
    END

    /* Insert TRN (Transaction) header records - one per check */
    INSERT INTO dbo.REMITTANCE_STAGING (
        payment_run_id, isa_control_nbr, gs_control_nbr, st_control_nbr,
        trn_reference_nbr, check_eft_nbr, payment_method_cd, payment_dt,
        payer_id, payer_name, payer_tax_id,
        payee_id, payee_name, payee_npi, payee_tax_id,
        claim_number, claim_suffix, member_id,
        charge_amt, allowed_amt, paid_amt, patient_resp_amt,
        segment_type_cd)
    SELECT @payment_run_id, @isa_control_nbr, @gs_control_nbr, @st_control_nbr,
           pb.check_eft_nbr, pb.check_eft_nbr, pb.payment_method_cd, @generation_dt,
           @payer_id, @payer_name, @payer_tax_id,
           pb.payee_id, pb.payee_name, pb.payee_npi, pb.payee_tax_id,
           'TRN', '00', NULL,
           pb.gross_amt, pb.gross_amt, pb.net_amt, 0.00,
           'TRN'
    FROM   #payee_bundles pb

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 7010, @o_return_message = 'ERROR: TRN staging insert failed'
        GOTO ERROR_EXIT
    END

    /* Insert CLP (Claim-level) records */
    INSERT INTO dbo.REMITTANCE_STAGING (
        payment_run_id, isa_control_nbr, gs_control_nbr, st_control_nbr,
        trn_reference_nbr, check_eft_nbr, payment_method_cd, payment_dt,
        payer_id, payer_name, payer_tax_id,
        payee_id, payee_name, payee_npi, payee_tax_id,
        claim_number, claim_suffix, member_id, member_name,
        patient_acct_nbr, claim_status_cd,
        charge_amt, allowed_amt, paid_amt, patient_resp_amt,
        deductible_amt, copay_amt, coinsurance_amt, cob_amt,
        withhold_amt, interest_amt,
        date_of_service_from, date_of_service_to,
        segment_type_cd)
    SELECT @payment_run_id, @isa_control_nbr, @gs_control_nbr, @st_control_nbr,
           pb.check_eft_nbr, pb.check_eft_nbr, pb.payment_method_cd, @generation_dt,
           @payer_id, @payer_name, @payer_tax_id,
           pb.payee_id, pb.payee_name, pb.payee_npi, pb.payee_tax_id,
           ch.claim_number, ch.claim_suffix,
           ch.member_id,
           (SELECT TOP 1 pm.last_name + ', ' + pm.first_name
            FROM dbo.PATIENT_MASTER pm (NOLOCK) WHERE pm.member_id = ch.member_id),
           ch.patient_acct_nbr,
           CASE ch.claim_status_cd
               WHEN 'PAID' THEN '1'    /* Processed as Primary */
               WHEN 'DENY' THEN '4'    /* Denied */
               WHEN 'ADJ'  THEN '22'   /* Reversal of Previous Payment */
               WHEN 'VOID' THEN '22'
               ELSE '1'
           END,
           ch.total_charge_amt, ch.total_allowed_amt,
           ch.total_paid_amt, ch.total_patient_resp_amt,
           ch.total_deductible_amt, ch.total_copay_amt,
           ch.total_coinsurance_amt, ch.total_cob_amt,
           ch.total_withhold_amt, ch.total_interest_amt,
           (SELECT MIN(cd.date_of_service_from) FROM dbo.CLAIM_DETAIL cd (NOLOCK)
            WHERE cd.claim_number = ch.claim_number AND cd.claim_suffix = ch.claim_suffix),
           (SELECT MAX(cd.date_of_service_to) FROM dbo.CLAIM_DETAIL cd (NOLOCK)
            WHERE cd.claim_number = ch.claim_number AND cd.claim_suffix = ch.claim_suffix),
           'CLP'
    FROM   dbo.PAYMENT_HISTORY ph (NOLOCK)
    JOIN   dbo.CLAIM_HEADER ch (NOLOCK) ON ch.claim_number = ph.claim_number
                                        AND ch.claim_suffix = ph.claim_suffix
    JOIN   #payee_bundles pb ON pb.payee_id = ph.payee_id
    WHERE  ph.payment_run_id = @payment_run_id
    AND    ph.edi_835_sent_flag = 'N'
    AND    ph.payment_type_cd IN ('PMT','ADJ','VOID')
    AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)
    GROUP BY ch.claim_number, ch.claim_suffix, ch.member_id,
             ch.patient_acct_nbr, ch.claim_status_cd,
             ch.total_charge_amt, ch.total_allowed_amt,
             ch.total_paid_amt, ch.total_patient_resp_amt,
             ch.total_deductible_amt, ch.total_copay_amt,
             ch.total_coinsurance_amt, ch.total_cob_amt,
             ch.total_withhold_amt, ch.total_interest_amt,
             pb.check_eft_nbr, pb.payment_method_cd,
             pb.payee_id, pb.payee_name, pb.payee_npi, pb.payee_tax_id

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 7020, @o_return_message = 'ERROR: CLP staging insert failed'
        GOTO ERROR_EXIT
    END

    /* Insert SVC (Service-level) records */
    INSERT INTO dbo.REMITTANCE_STAGING (
        payment_run_id, isa_control_nbr, gs_control_nbr, st_control_nbr,
        trn_reference_nbr, check_eft_nbr, payment_method_cd, payment_dt,
        payer_id, payee_id,
        claim_number, claim_suffix, line_nbr, member_id,
        charge_amt, allowed_amt, paid_amt, patient_resp_amt,
        deductible_amt, copay_amt, coinsurance_amt, cob_amt,
        carc_cd, rarc_cd, rarc_cd_2, grp_cd,
        date_of_service_from, date_of_service_to,
        proc_cd, modifier_1, revenue_cd, units_of_service,
        segment_type_cd)
    SELECT @payment_run_id, @isa_control_nbr, @gs_control_nbr, @st_control_nbr,
           pb.check_eft_nbr, pb.check_eft_nbr, pb.payment_method_cd, @generation_dt,
           @payer_id, pb.payee_id,
           cd.claim_number, cd.claim_suffix, cd.line_nbr, ch.member_id,
           cd.charge_amt, cd.allowed_amt, cd.paid_amt, cd.patient_resp_amt,
           cd.deductible_amt, cd.copay_amt, cd.coinsurance_amt, cd.cob_amt,
           cd.carc_cd, cd.rarc_cd, cd.rarc_cd_2, cd.grp_cd,
           cd.date_of_service_from, cd.date_of_service_to,
           cd.proc_cd, cd.modifier_1, cd.revenue_cd, cd.units_of_service,
           'SVC'
    FROM   dbo.CLAIM_DETAIL cd (NOLOCK)
    JOIN   dbo.CLAIM_HEADER ch (NOLOCK) ON ch.claim_number = cd.claim_number
                                        AND ch.claim_suffix = cd.claim_suffix
    JOIN   dbo.PAYMENT_HISTORY ph (NOLOCK) ON ph.claim_number = cd.claim_number
                                           AND ph.claim_suffix = cd.claim_suffix
    JOIN   #payee_bundles pb ON pb.payee_id = ph.payee_id
    WHERE  ph.payment_run_id = @payment_run_id
    AND    ph.edi_835_sent_flag = 'N'
    AND    ph.payment_type_cd IN ('PMT','ADJ','VOID')
    AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 7030, @o_return_message = 'ERROR: SVC staging insert failed'
        GOTO ERROR_EXIT
    END

    /* Insert PLB (Provider-Level Balance) records */
    INSERT INTO dbo.REMITTANCE_STAGING (
        payment_run_id, isa_control_nbr, gs_control_nbr, st_control_nbr,
        trn_reference_nbr, check_eft_nbr, payment_method_cd, payment_dt,
        payer_id, payee_id, plb_provider_id,
        claim_number, claim_suffix,
        plb_adj_reason_cd, plb_adj_amt,
        segment_type_cd)
    SELECT @payment_run_id, @isa_control_nbr, @gs_control_nbr, @st_control_nbr,
           pb.check_eft_nbr, pb.check_eft_nbr, pb.payment_method_cd, @generation_dt,
           @payer_id, pa.payee_id, pa.payee_id,
           'PLB', '00',
           pa.plb_reason_cd, pa.plb_amount,
           'PLB'
    FROM   #plb_adjustments pa
    JOIN   #payee_bundles pb ON pb.payee_id = pa.payee_id

    SELECT @err_code = @@error  /* PLB insert is non-critical */

    /***************************************************************************
     * STEP 8: MARK RECORDS AS GENERATED
     ***************************************************************************/
    SELECT @step_name = 'MARK_SENT'

    IF @test_mode = 'N'
    BEGIN
        /* Mark payment history as sent */
        UPDATE ph
        SET    ph.edi_835_sent_flag = 'Y',
               ph.edi_835_sent_dt  = @generation_dt,
               ph.edi_835_control_nbr = @isa_control_nbr,
               ph.modified_by = SUSER_NAME(),
               ph.modified_dt = GETDATE()
        FROM   dbo.PAYMENT_HISTORY ph
        WHERE  ph.payment_run_id = @payment_run_id
        AND    ph.edi_835_sent_flag = 'N'
        AND    (@payee_id IS NULL OR ph.payee_id = @payee_id)

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 8001, @o_return_message = 'ERROR: Failed to mark payments as sent'
            GOTO ERROR_EXIT
        END

        /* Mark staging as generated */
        UPDATE dbo.REMITTANCE_STAGING
        SET    generated_flag = 'Y',
               generated_dt = @generation_dt
        WHERE  payment_run_id = @payment_run_id
        AND    isa_control_nbr = @isa_control_nbr

        SELECT @err_code = @@error
        IF @err_code != 0
        BEGIN
            SELECT @o_return_code = 8002, @o_return_message = 'ERROR: Failed to mark staging as generated'
            GOTO ERROR_EXIT
        END
    END

    /***************************************************************************
     * STEP 9: INSERT EDI 835 CONTROL RECORD
     ***************************************************************************/
    SELECT @step_name = 'CONTROL_REC'

    SELECT @file_name = '835_' + @payment_run_id + '_' + @isa_control_nbr + '_' +
                        CONVERT(varchar, @generation_dt, 112) + '.txt'

    SELECT @o_check_count  = COUNT(*) FROM #payee_bundles
    SELECT @o_claim_count  = SUM(claim_count) FROM #payee_bundles
    SELECT @o_total_paid_amt = SUM(net_amt) FROM #payee_bundles
    SELECT @o_file_count   = 1

    INSERT INTO dbo.EDI_835_CONTROL (
        payment_run_id, isa_control_nbr, gs_control_nbr, st_control_nbr,
        sender_id, receiver_id, file_name,
        total_payment_amt, total_claim_count, total_check_count,
        generation_dt, transmission_status_cd)
    VALUES (
        @payment_run_id, @isa_control_nbr, @gs_control_nbr, @st_control_nbr,
        @edi_sender_id, ISNULL(@edi_receiver_id, 'UNKNOWN'), @file_name,
        @o_total_paid_amt, @o_claim_count, @o_check_count,
        @generation_dt,
        CASE @test_mode WHEN 'Y' THEN 'PEND' ELSE 'PEND' END)

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 8010, @o_return_message = 'ERROR: 835 control record insert failed'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 10: INSERT CHECK REGISTER RECORDS
     ***************************************************************************/
    SELECT @step_name = 'CHECK_REG'

    INSERT INTO dbo.CHECK_REGISTER (
        check_eft_nbr, payment_method_cd, payment_run_id, payment_dt,
        payee_id, payee_name, payee_tax_id,
        payee_address_1, payee_city, payee_state_cd, payee_zip_cd,
        gross_amt, withhold_amt, plb_adj_amt, net_amt,
        claim_count, line_count, status_cd, bank_account_cd)
    SELECT pb.check_eft_nbr, pb.payment_method_cd, @payment_run_id, @generation_dt,
           pb.payee_id, pb.payee_name, pb.payee_tax_id,
           pb.payee_address_1, pb.payee_city, pb.payee_state_cd, pb.payee_zip_cd,
           pb.gross_amt, pb.withhold_amt, pb.plb_adj_amt, pb.net_amt,
           pb.claim_count, pb.line_count, 'ISSUE', 'OPER01'
    FROM   #payee_bundles pb
    WHERE  NOT EXISTS (
        SELECT 1 FROM dbo.CHECK_REGISTER cr
        WHERE cr.check_eft_nbr = pb.check_eft_nbr
        AND cr.payment_run_id = @payment_run_id)

    SELECT @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 8020, @o_return_message = 'ERROR: Check register insert failed'
        GOTO ERROR_EXIT
    END

    /***************************************************************************
     * STEP 11: COMMIT TRANSACTION
     ***************************************************************************/
    SELECT @step_name = 'COMMIT'
    COMMIT TRAN GEN_835
    SELECT @tran_active = 0, @err_code = @@error
    IF @err_code != 0
    BEGIN
        SELECT @o_return_code = 9010, @o_return_message = 'ERROR: Commit failed'
        GOTO ERROR_EXIT
    END

    SELECT @o_return_code = 0,
           @o_return_message = '835 generated: Checks=' + CONVERT(varchar, @o_check_count) +
               ' Claims=' + CONVERT(varchar, @o_claim_count) +
               ' Total=$' + CONVERT(varchar, @o_total_paid_amt) +
               ' File=' + @file_name +
               ' ISA=' + @isa_control_nbr

    GOTO CLEAN_EXIT

    /***************************************************************************
     * ERROR EXIT
     ***************************************************************************/
    ERROR_EXIT:
        IF @tran_active = 1
        BEGIN
            ROLLBACK TRAN GEN_835
            SELECT @tran_active = 0
        END

    /***************************************************************************
     * CLEAN EXIT
     ***************************************************************************/
    CLEAN_EXIT:
        IF OBJECT_ID('tempdb..#payee_bundles') IS NOT NULL
            DROP TABLE #payee_bundles
        IF OBJECT_ID('tempdb..#plb_adjustments') IS NOT NULL
            DROP TABLE #plb_adjustments

    RETURN @o_return_code
END
go

PRINT 'Stored procedure SP_GENERATE_835 created successfully.'
go

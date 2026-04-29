/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Stored Procedure: SP_CLAIM_PRICING
 *
 * Purpose  : Comprehensive claim pricing engine supporting multiple
 *            methodologies: fee schedule, percent of charge, per diem,
 *            DRG, case rate, Medicare RBRVS, and outpatient APC grouping.
 *            Handles modifier-based adjustments and GPCI locality factors.
 *
 * Database : HCPS_DB
 * Platform : Sybase ASE
 * Version  : 1.0
 *
 * Parameters:
 *   @p_hcpcs_code      - HCPCS/CPT procedure code
 *   @p_modifier        - Primary modifier (nullable)
 *   @p_pos             - Place of service code
 *   @p_locality        - Geographic locality code for GPCI
 *   @p_fee_sched_id    - Fee schedule identifier
 *   @p_charge_amt      - Billed charge amount
 *   @p_units           - Service units
 *   @p_dos             - Date of service
 *   @p_reimb_method    - Reimbursement method override (nullable)
 *   @p_reimb_pct       - Reimbursement percentage override (nullable)
 *   @p_allowed_amt     - Output: calculated allowed amount
 *   @p_price_method    - Output: pricing method used
 *   @p_return_code     - Output: 0=Success, 1=Warning, -1=Error
 *   @p_return_msg      - Output: Status/error message
 ******************************************************************************/

use HCPS_DB
go

if exists (select 1 from sysobjects where name = 'SP_CLAIM_PRICING' and type = 'P')
    drop procedure SP_CLAIM_PRICING
go

create procedure SP_CLAIM_PRICING
    @p_hcpcs_code      varchar(5),
    @p_modifier         char(2)        = null,
    @p_pos              char(2)        = null,
    @p_locality         varchar(5)     = null,
    @p_fee_sched_id     varchar(15),
    @p_charge_amt       numeric(13,2),
    @p_units            numeric(9,2),
    @p_dos              datetime,
    @p_reimb_method     char(2)        = null,
    @p_reimb_pct        numeric(9,4)   = null,
    @p_allowed_amt      numeric(13,2)  output,
    @p_price_method     char(2)        output,
    @p_return_code      int            output,
    @p_return_msg       varchar(255)   output
as
begin
    /* -------------------------------------------------------------------
     * Local variable declarations
     * ---------------------------------------------------------------- */
    /* Fee schedule lookup results */
    declare @v_fs_allowed          numeric(13,2)
    declare @v_fs_work_rvu         numeric(9,4)
    declare @v_fs_pe_rvu           numeric(9,4)
    declare @v_fs_mp_rvu           numeric(9,4)
    declare @v_fs_total_rvu        numeric(9,4)
    declare @v_fs_conv_factor      numeric(9,4)
    declare @v_fs_gpci_work        numeric(9,4)
    declare @v_fs_gpci_pe          numeric(9,4)
    declare @v_fs_gpci_mp          numeric(9,4)
    declare @v_fs_price_method     char(2)
    declare @v_fs_pct_allowed      numeric(9,4)
    declare @v_fs_per_diem         numeric(13,2)
    declare @v_fs_case_rate        numeric(13,2)
    declare @v_fs_unit_rate        numeric(13,2)
    declare @v_fs_max_units        numeric(9,2)
    declare @v_fs_floor_amt        numeric(13,2)
    declare @v_fs_ceiling_amt      numeric(13,2)
    declare @v_fs_found            char(1)

    /* HCPCS/CPT code attributes */
    declare @v_hcpcs_type          char(1)
    declare @v_hcpcs_work_rvu      numeric(9,4)
    declare @v_hcpcs_pe_rvu        numeric(9,4)
    declare @v_hcpcs_mp_rvu        numeric(9,4)
    declare @v_hcpcs_total_rvu     numeric(9,4)
    declare @v_hcpcs_conv_factor   numeric(9,4)
    declare @v_hcpcs_mult_proc     char(1)
    declare @v_hcpcs_bilat_surg    char(1)
    declare @v_hcpcs_asst_surg     char(1)
    declare @v_hcpcs_global_days   varchar(3)
    declare @v_hcpcs_found         char(1)

    /* Modifier adjustment factors */
    declare @v_mod_adj_factor      numeric(9,4)
    declare @v_mod_26_flag         char(1)       /* Professional component */
    declare @v_mod_tc_flag         char(1)       /* Technical component */
    declare @v_mod_50_flag         char(1)       /* Bilateral */
    declare @v_mod_51_flag         char(1)       /* Multiple procedures */
    declare @v_mod_59_flag         char(1)       /* Distinct procedure */
    declare @v_mod_80_flag         char(1)       /* Assistant surgeon */
    declare @v_mod_as_flag         char(1)       /* NP/PA assistant */
    declare @v_mod_reduced_flag    char(1)       /* 52 = Reduced services */
    declare @v_mod_62_flag         char(1)       /* Two surgeons */

    /* GPCI / RBRVS calculation variables */
    declare @v_gpci_work           numeric(9,4)
    declare @v_gpci_pe             numeric(9,4)
    declare @v_gpci_mp             numeric(9,4)
    declare @v_rbrvs_work_adj      numeric(13,4)
    declare @v_rbrvs_pe_adj        numeric(13,4)
    declare @v_rbrvs_mp_adj        numeric(13,4)
    declare @v_rbrvs_total_rvu     numeric(13,4)
    declare @v_rbrvs_payment       numeric(13,2)
    declare @v_conversion_factor   numeric(9,4)

    /* DRG variables */
    declare @v_drg_code            char(4)
    declare @v_drg_weight          numeric(9,4)
    declare @v_drg_base_rate       numeric(13,2)
    declare @v_drg_payment         numeric(13,2)

    /* APC variables */
    declare @v_apc_group           varchar(10)
    declare @v_apc_weight          numeric(9,4)
    declare @v_apc_rate            numeric(13,2)
    declare @v_apc_payment         numeric(13,2)
    declare @v_apc_found           char(1)

    /* Per diem variables */
    declare @v_per_diem_rate       numeric(13,2)
    declare @v_per_diem_days       smallint

    /* Case rate variables */
    declare @v_case_rate           numeric(13,2)

    /* Working calculation variables */
    declare @v_calc_allowed        numeric(13,2)
    declare @v_adjusted_units      numeric(9,2)
    declare @v_final_method        char(2)
    declare @v_error_code          int

    /* -------------------------------------------------------------------
     * Initialize
     * ---------------------------------------------------------------- */
    select @p_allowed_amt    = 0.00
    select @p_price_method   = null
    select @p_return_code    = 0
    select @p_return_msg     = ''
    select @v_fs_found       = 'N'
    select @v_hcpcs_found    = 'N'
    select @v_apc_found      = 'N'
    select @v_mod_adj_factor = 1.0000
    select @v_mod_26_flag    = 'N'
    select @v_mod_tc_flag    = 'N'
    select @v_mod_50_flag    = 'N'
    select @v_mod_51_flag    = 'N'
    select @v_mod_59_flag    = 'N'
    select @v_mod_80_flag    = 'N'
    select @v_mod_as_flag    = 'N'
    select @v_mod_reduced_flag = 'N'
    select @v_mod_62_flag    = 'N'
    select @v_calc_allowed   = 0.00
    select @v_adjusted_units = isnull(@p_units, 1.00)

    /* -------------------------------------------------------------------
     * STEP 1: Validate HCPCS/CPT code exists and get attributes
     * ---------------------------------------------------------------- */
    select @v_hcpcs_type        = HCPCS_TYPE,
           @v_hcpcs_work_rvu    = HCPCS_WORK_RVU,
           @v_hcpcs_pe_rvu      = HCPCS_PE_RVU,
           @v_hcpcs_mp_rvu      = HCPCS_MP_RVU,
           @v_hcpcs_total_rvu   = HCPCS_TOTAL_RVU,
           @v_hcpcs_conv_factor = HCPCS_CONV_FACTOR,
           @v_hcpcs_mult_proc   = HCPCS_MULT_PROC_IND,
           @v_hcpcs_bilat_surg  = HCPCS_BILAT_SURG_IND,
           @v_hcpcs_asst_surg   = HCPCS_ASST_SURG_IND,
           @v_hcpcs_global_days = HCPCS_GLOBAL_DAYS
      from HCPCS_CPT
     where HCPCS_CODE      = @p_hcpcs_code
       and HCPCS_STATUS_IND = 'A'
       and HCPCS_EFF_DATE  <= @p_dos
       and (HCPCS_TERM_DATE is null or HCPCS_TERM_DATE >= @p_dos)

    if @@rowcount > 0
    begin
        select @v_hcpcs_found = 'Y'
    end
    else
    begin
        select @p_return_code = 1
        select @p_return_msg  = 'WARNING: HCPCS code ' + @p_hcpcs_code +
            ' not found or inactive for DOS'
        /* Continue with fee schedule lookup - code may still be priced */
    end

    /* -------------------------------------------------------------------
     * STEP 2: Parse modifier and determine adjustment factors
     * ---------------------------------------------------------------- */
    if @p_modifier is not null
    begin
        if @p_modifier = '26'
        begin
            /* Professional component only */
            select @v_mod_26_flag    = 'Y'
            select @v_mod_adj_factor = 1.0000  /* Use PE RVU only */
        end
        else if @p_modifier = 'TC'
        begin
            /* Technical component only */
            select @v_mod_tc_flag    = 'Y'
            select @v_mod_adj_factor = 1.0000
        end
        else if @p_modifier = '50'
        begin
            /* Bilateral procedure - pay 150% */
            select @v_mod_50_flag    = 'Y'
            select @v_mod_adj_factor = 1.5000
        end
        else if @p_modifier = '51'
        begin
            /* Multiple procedures - reduce by 50% for 2nd+ proc */
            select @v_mod_51_flag    = 'Y'
            select @v_mod_adj_factor = 0.5000
        end
        else if @p_modifier = '52'
        begin
            /* Reduced services */
            select @v_mod_reduced_flag = 'Y'
            select @v_mod_adj_factor   = 0.8000
        end
        else if @p_modifier = '59'
        begin
            /* Distinct procedural service - no reduction */
            select @v_mod_59_flag    = 'Y'
            select @v_mod_adj_factor = 1.0000
        end
        else if @p_modifier = '62'
        begin
            /* Two surgeons - each gets 62.5% */
            select @v_mod_62_flag    = 'Y'
            select @v_mod_adj_factor = 0.6250
        end
        else if @p_modifier = '80'
        begin
            /* Assistant surgeon - 16% */
            select @v_mod_80_flag    = 'Y'
            if @v_hcpcs_asst_surg = '0' or @v_hcpcs_asst_surg = 'N'
            begin
                /* Assistant not payable */
                select @v_mod_adj_factor = 0.0000
            end
            else
            begin
                select @v_mod_adj_factor = 0.1600
            end
        end
        else if @p_modifier = 'AS'
        begin
            /* NP/PA/CNS assistant at surgery - 13.6% */
            select @v_mod_as_flag    = 'Y'
            select @v_mod_adj_factor = 0.1360
        end
        else if @p_modifier = '76'
        begin
            /* Repeat procedure by same physician - full rate */
            select @v_mod_adj_factor = 1.0000
        end
        else if @p_modifier = '77'
        begin
            /* Repeat procedure by different physician - full rate */
            select @v_mod_adj_factor = 1.0000
        end
        else if @p_modifier = '22'
        begin
            /* Increased procedural services - 120% */
            select @v_mod_adj_factor = 1.2000
        end
        else if @p_modifier = '53'
        begin
            /* Discontinued procedure - 50% */
            select @v_mod_adj_factor = 0.5000
        end
        else
        begin
            /* Unknown modifier - default to 100% */
            select @v_mod_adj_factor = 1.0000
        end
    end

    /* -------------------------------------------------------------------
     * STEP 3: Determine pricing methodology
     * ---------------------------------------------------------------- */
    declare @v_price_method char(2)

    /* Priority: provider contract override > payer config > fee schedule */
    if @p_reimb_method is not null
    begin
        select @v_price_method = @p_reimb_method
    end
    else
    begin
        /* Look up from fee schedule */
        select @v_price_method = 'FS'  /* Default to fee schedule */
    end

    /* -------------------------------------------------------------------
     * STEP 4: Fee Schedule Lookup
     * ---------------------------------------------------------------- */
    /* Try exact match first: code + modifier + POS + locality */
    select @v_fs_allowed      = FEESCHED_ALLOWED_AMT,
           @v_fs_work_rvu     = FEESCHED_WORK_RVU,
           @v_fs_pe_rvu       = FEESCHED_PE_RVU,
           @v_fs_mp_rvu       = FEESCHED_MP_RVU,
           @v_fs_total_rvu    = FEESCHED_TOTAL_RVU,
           @v_fs_conv_factor  = FEESCHED_CONV_FACTOR,
           @v_fs_gpci_work    = FEESCHED_GPCI_WORK,
           @v_fs_gpci_pe      = FEESCHED_GPCI_PE,
           @v_fs_gpci_mp      = FEESCHED_GPCI_MP,
           @v_fs_price_method = FEESCHED_PRICE_METHOD,
           @v_fs_pct_allowed  = FEESCHED_PCT_ALLOWED,
           @v_fs_per_diem     = FEESCHED_PER_DIEM_RATE,
           @v_fs_case_rate    = FEESCHED_CASE_RATE,
           @v_fs_unit_rate    = FEESCHED_UNIT_RATE,
           @v_fs_max_units    = FEESCHED_MAX_UNITS,
           @v_fs_floor_amt    = FEESCHED_FLOOR_AMT,
           @v_fs_ceiling_amt  = FEESCHED_CEILING_AMT
      from FEE_SCHEDULE
     where FEESCHED_ID        = @p_fee_sched_id
       and FEESCHED_HCPCS_CODE = @p_hcpcs_code
       and FEESCHED_MODIFIER   = isnull(@p_modifier, FEESCHED_MODIFIER)
       and FEESCHED_POS        = isnull(@p_pos, FEESCHED_POS)
       and FEESCHED_LOCALITY   = isnull(@p_locality, FEESCHED_LOCALITY)
       and FEESCHED_EFF_DATE  <= @p_dos
       and (FEESCHED_TERM_DATE is null or FEESCHED_TERM_DATE >= @p_dos)
       and FEESCHED_STATUS = 'A'

    if @@rowcount > 0
    begin
        select @v_fs_found = 'Y'
        if @v_fs_price_method is not null
            select @v_price_method = @v_fs_price_method
    end
    else
    begin
        /* Try without modifier */
        select @v_fs_allowed      = FEESCHED_ALLOWED_AMT,
               @v_fs_work_rvu     = FEESCHED_WORK_RVU,
               @v_fs_pe_rvu       = FEESCHED_PE_RVU,
               @v_fs_mp_rvu       = FEESCHED_MP_RVU,
               @v_fs_total_rvu    = FEESCHED_TOTAL_RVU,
               @v_fs_conv_factor  = FEESCHED_CONV_FACTOR,
               @v_fs_gpci_work    = FEESCHED_GPCI_WORK,
               @v_fs_gpci_pe      = FEESCHED_GPCI_PE,
               @v_fs_gpci_mp      = FEESCHED_GPCI_MP,
               @v_fs_price_method = FEESCHED_PRICE_METHOD,
               @v_fs_pct_allowed  = FEESCHED_PCT_ALLOWED,
               @v_fs_per_diem     = FEESCHED_PER_DIEM_RATE,
               @v_fs_case_rate    = FEESCHED_CASE_RATE,
               @v_fs_unit_rate    = FEESCHED_UNIT_RATE,
               @v_fs_max_units    = FEESCHED_MAX_UNITS,
               @v_fs_floor_amt    = FEESCHED_FLOOR_AMT,
               @v_fs_ceiling_amt  = FEESCHED_CEILING_AMT
          from FEE_SCHEDULE
         where FEESCHED_ID         = @p_fee_sched_id
           and FEESCHED_HCPCS_CODE = @p_hcpcs_code
           and (FEESCHED_MODIFIER is null or FEESCHED_MODIFIER = '  ')
           and FEESCHED_EFF_DATE  <= @p_dos
           and (FEESCHED_TERM_DATE is null or FEESCHED_TERM_DATE >= @p_dos)
           and FEESCHED_STATUS = 'A'

        if @@rowcount > 0
        begin
            select @v_fs_found = 'Y'
            if @v_fs_price_method is not null
                select @v_price_method = @v_fs_price_method
        end
        else
        begin
            /* Try STANDARD fee schedule as fallback */
            if @p_fee_sched_id != 'STANDARD'
            begin
                select @v_fs_allowed      = FEESCHED_ALLOWED_AMT,
                       @v_fs_work_rvu     = FEESCHED_WORK_RVU,
                       @v_fs_pe_rvu       = FEESCHED_PE_RVU,
                       @v_fs_mp_rvu       = FEESCHED_MP_RVU,
                       @v_fs_total_rvu    = FEESCHED_TOTAL_RVU,
                       @v_fs_conv_factor  = FEESCHED_CONV_FACTOR,
                       @v_fs_gpci_work    = FEESCHED_GPCI_WORK,
                       @v_fs_gpci_pe      = FEESCHED_GPCI_PE,
                       @v_fs_gpci_mp      = FEESCHED_GPCI_MP,
                       @v_fs_price_method = FEESCHED_PRICE_METHOD,
                       @v_fs_pct_allowed  = FEESCHED_PCT_ALLOWED,
                       @v_fs_per_diem     = FEESCHED_PER_DIEM_RATE,
                       @v_fs_case_rate    = FEESCHED_CASE_RATE,
                       @v_fs_unit_rate    = FEESCHED_UNIT_RATE,
                       @v_fs_max_units    = FEESCHED_MAX_UNITS,
                       @v_fs_floor_amt    = FEESCHED_FLOOR_AMT,
                       @v_fs_ceiling_amt  = FEESCHED_CEILING_AMT
                  from FEE_SCHEDULE
                 where FEESCHED_ID         = 'STANDARD'
                   and FEESCHED_HCPCS_CODE = @p_hcpcs_code
                   and FEESCHED_EFF_DATE  <= @p_dos
                   and (FEESCHED_TERM_DATE is null or FEESCHED_TERM_DATE >= @p_dos)
                   and FEESCHED_STATUS = 'A'

                if @@rowcount > 0
                begin
                    select @v_fs_found = 'Y'
                    if @v_fs_price_method is not null
                        select @v_price_method = @v_fs_price_method
                end
            end
        end
    end

    /* -------------------------------------------------------------------
     * STEP 5: Calculate allowed amount based on pricing method
     * ---------------------------------------------------------------- */

    /* ----- METHOD FS: Fee Schedule fixed amount ----- */
    if @v_price_method = 'FS'
    begin
        if @v_fs_found = 'Y' and @v_fs_allowed is not null
        begin
            select @v_calc_allowed = @v_fs_allowed * @v_adjusted_units
            select @v_final_method = 'FS'
        end
        else
        begin
            /* No fee schedule entry - use percent of charge as fallback */
            select @v_price_method = 'PC'
        end
    end

    /* ----- METHOD RV: RVU-based / Medicare RBRVS Calculation ----- */
    if @v_price_method = 'RV'
    begin
        /* Get GPCI factors for locality */
        select @v_gpci_work = isnull(@v_fs_gpci_work, 1.0000)
        select @v_gpci_pe   = isnull(@v_fs_gpci_pe,   1.0000)
        select @v_gpci_mp   = isnull(@v_fs_gpci_mp,   1.0000)

        /* Use fee schedule RVUs if available, else HCPCS table RVUs */
        declare @v_work_rvu numeric(9,4)
        declare @v_pe_rvu   numeric(9,4)
        declare @v_mp_rvu   numeric(9,4)

        if @v_fs_found = 'Y' and @v_fs_work_rvu is not null
        begin
            select @v_work_rvu = @v_fs_work_rvu
            select @v_pe_rvu   = isnull(@v_fs_pe_rvu, 0.0000)
            select @v_mp_rvu   = isnull(@v_fs_mp_rvu, 0.0000)
        end
        else if @v_hcpcs_found = 'Y'
        begin
            select @v_work_rvu = isnull(@v_hcpcs_work_rvu, 0.0000)
            select @v_pe_rvu   = isnull(@v_hcpcs_pe_rvu,   0.0000)
            select @v_mp_rvu   = isnull(@v_hcpcs_mp_rvu,   0.0000)
        end
        else
        begin
            /* No RVU data available */
            select @p_return_code = 1
            select @p_return_msg  = 'WARNING: No RVU data for RBRVS calc - ' +
                @p_hcpcs_code
            select @v_price_method = 'PC'  /* Fallback to % of charge */
        end

        if @v_price_method = 'RV'
        begin
            /* Handle professional/technical component modifiers */
            if @v_mod_26_flag = 'Y'
            begin
                /* Professional component: Work RVU + portion of PE */
                select @v_rbrvs_work_adj = @v_work_rvu * @v_gpci_work
                select @v_rbrvs_pe_adj   = 0.0000   /* No facility PE */
                select @v_rbrvs_mp_adj   = @v_mp_rvu * @v_gpci_mp
            end
            else if @v_mod_tc_flag = 'Y'
            begin
                /* Technical component: PE only */
                select @v_rbrvs_work_adj = 0.0000
                select @v_rbrvs_pe_adj   = @v_pe_rvu * @v_gpci_pe
                select @v_rbrvs_mp_adj   = 0.0000
            end
            else
            begin
                /* Global service - all components */
                select @v_rbrvs_work_adj = @v_work_rvu * @v_gpci_work
                select @v_rbrvs_pe_adj   = @v_pe_rvu   * @v_gpci_pe
                select @v_rbrvs_mp_adj   = @v_mp_rvu   * @v_gpci_mp
            end

            /* Sum geographically adjusted RVUs */
            select @v_rbrvs_total_rvu = @v_rbrvs_work_adj +
                @v_rbrvs_pe_adj + @v_rbrvs_mp_adj

            /* Apply conversion factor */
            select @v_conversion_factor = isnull(@v_fs_conv_factor,
                isnull(@v_hcpcs_conv_factor, 36.0896))

            /* Medicare RBRVS formula:
               Payment = [(Work RVU * Work GPCI) +
                          (PE RVU * PE GPCI) +
                          (MP RVU * MP GPCI)] * Conversion Factor */
            select @v_rbrvs_payment = @v_rbrvs_total_rvu *
                @v_conversion_factor

            select @v_calc_allowed = @v_rbrvs_payment * @v_adjusted_units
            select @v_final_method = 'RV'
        end
    end

    /* ----- METHOD PC: Percent of Charge ----- */
    if @v_price_method = 'PC'
    begin
        declare @v_pct numeric(9,4)

        /* Use override percentage, fee schedule pct, or default */
        if @p_reimb_pct is not null and @p_reimb_pct > 0
        begin
            select @v_pct = @p_reimb_pct
        end
        else if @v_fs_found = 'Y' and @v_fs_pct_allowed is not null
        begin
            select @v_pct = @v_fs_pct_allowed
        end
        else
        begin
            select @v_pct = 80.0000   /* Default 80% of charge */
        end

        select @v_calc_allowed = @p_charge_amt * (@v_pct / 100.0000)
        select @v_final_method = 'PC'
    end

    /* ----- METHOD PD: Per Diem ----- */
    if @v_price_method = 'PD'
    begin
        if @v_fs_found = 'Y' and @v_fs_per_diem is not null
        begin
            select @v_per_diem_rate = @v_fs_per_diem
        end
        else
        begin
            /* No per diem rate found - use charge as fallback */
            select @v_per_diem_rate = @p_charge_amt
        end

        /* Units represent days for per diem */
        select @v_per_diem_days = convert(smallint, @v_adjusted_units)
        if @v_per_diem_days < 1
            select @v_per_diem_days = 1

        select @v_calc_allowed = @v_per_diem_rate * @v_per_diem_days
        select @v_final_method = 'PD'
    end

    /* ----- METHOD DG: DRG-Based Payment ----- */
    if @v_price_method = 'DG'
    begin
        /* DRG pricing is typically handled at the claim level,
           but we support line-level DRG lookup as well */
        select @v_drg_weight    = DRG_WEIGHT,
               @v_drg_base_rate = isnull(DRG_BASE_RATE, 0.00)
          from DRG_WEIGHT dw
         where exists (
                select 1 from HCPCS_CPT h
                 where h.HCPCS_CODE = @p_hcpcs_code
               )
           and dw.DRG_EFF_DATE <= @p_dos
           and (dw.DRG_TERM_DATE is null or dw.DRG_TERM_DATE >= @p_dos)

        if @@rowcount > 0
        begin
            select @v_drg_payment = @v_drg_weight * @v_drg_base_rate
            select @v_calc_allowed = @v_drg_payment
            select @v_final_method = 'DG'
        end
        else
        begin
            /* No DRG match - fallback to fee schedule or % of charge */
            select @v_price_method = 'PC'
            select @v_calc_allowed = @p_charge_amt * 0.8000
            select @v_final_method = 'PC'
        end
    end

    /* ----- METHOD CR: Case Rate ----- */
    if @v_price_method = 'CR'
    begin
        if @v_fs_found = 'Y' and @v_fs_case_rate is not null
        begin
            select @v_case_rate = @v_fs_case_rate
        end
        else
        begin
            select @v_case_rate = @p_charge_amt
        end

        /* Case rate ignores units - single flat payment */
        select @v_calc_allowed = @v_case_rate
        select @v_final_method = 'CR'
    end

    /* ----- METHOD UR: Unit Rate ----- */
    if @v_price_method = 'UR'
    begin
        if @v_fs_found = 'Y' and @v_fs_unit_rate is not null
        begin
            select @v_calc_allowed = @v_fs_unit_rate * @v_adjusted_units
        end
        else
        begin
            select @v_calc_allowed = @p_charge_amt
        end
        select @v_final_method = 'UR'
    end

    /* ----- METHOD AP: Outpatient APC Grouping ----- */
    if @v_price_method = 'AP'
    begin
        /* APC lookup by HCPCS code */
        /* Sybase temp table for APC grouping logic */
        create table #apc_temp
        (
            apc_group    varchar(10)  null,
            apc_weight   numeric(9,4) null,
            apc_rate     numeric(13,2) null,
            apc_si       char(2)      null     /* Status indicator */
        )

        /* In a real system this would query an APC reference table.
           Simplified logic: use fee schedule unit rate as APC rate */
        if @v_fs_found = 'Y' and @v_fs_unit_rate is not null
        begin
            insert into #apc_temp values
                ('APC', 1.0000, @v_fs_unit_rate, 'S')
        end

        select @v_apc_rate = apc_rate,
               @v_apc_weight = apc_weight
          from #apc_temp

        if @@rowcount > 0 and @v_apc_rate is not null
        begin
            select @v_apc_payment = @v_apc_rate * isnull(@v_apc_weight, 1.0000)
            select @v_calc_allowed = @v_apc_payment
            select @v_final_method = 'AP'
            select @v_apc_found = 'Y'
        end
        else
        begin
            /* APC not found - fallback */
            select @v_calc_allowed = @p_charge_amt * 0.8000
            select @v_final_method = 'PC'
        end

        drop table #apc_temp
    end

    /* -------------------------------------------------------------------
     * STEP 6: Apply modifier adjustment factor
     * ---------------------------------------------------------------- */
    if @v_mod_adj_factor != 1.0000
    begin
        select @v_calc_allowed = @v_calc_allowed * @v_mod_adj_factor
    end

    /* -------------------------------------------------------------------
     * STEP 7: Apply provider contract reimbursement percentage override
     * ---------------------------------------------------------------- */
    if @p_reimb_pct is not null and @p_reimb_pct > 0.0000
       and @v_final_method != 'PC'     /* Avoid double application */
       and @p_reimb_method = 'PC'
    begin
        select @v_calc_allowed = @v_calc_allowed *
            (@p_reimb_pct / 100.0000)
    end

    /* -------------------------------------------------------------------
     * STEP 8: Enforce max units limit
     * ---------------------------------------------------------------- */
    if @v_fs_max_units is not null and @v_adjusted_units > @v_fs_max_units
    begin
        /* Recalculate with max units */
        if @v_final_method in ('FS', 'RV', 'UR')
        begin
            select @v_calc_allowed = (@v_calc_allowed / @v_adjusted_units)
                * @v_fs_max_units
        end

        select @p_return_msg = 'Units capped at ' +
            convert(varchar(10), @v_fs_max_units)
    end

    /* -------------------------------------------------------------------
     * STEP 9: Apply floor and ceiling amounts
     * ---------------------------------------------------------------- */
    if @v_fs_floor_amt is not null and @v_calc_allowed < @v_fs_floor_amt
    begin
        select @v_calc_allowed = @v_fs_floor_amt
    end

    if @v_fs_ceiling_amt is not null and @v_calc_allowed > @v_fs_ceiling_amt
    begin
        select @v_calc_allowed = @v_fs_ceiling_amt
    end

    /* -------------------------------------------------------------------
     * STEP 10: Allowed amount cannot exceed billed charge
     * ---------------------------------------------------------------- */
    if @v_calc_allowed > @p_charge_amt
    begin
        select @v_calc_allowed = @p_charge_amt
    end

    /* Ensure non-negative */
    if @v_calc_allowed < 0.00
    begin
        select @v_calc_allowed = 0.00
    end

    /* -------------------------------------------------------------------
     * STEP 11: Set output parameters
     * ---------------------------------------------------------------- */
    select @p_allowed_amt  = @v_calc_allowed
    select @p_price_method = @v_final_method

    if @p_return_code = 0
    begin
        select @p_return_msg = 'Pricing complete. Method: ' + @v_final_method +
            ', Allowed: $' + convert(varchar(15), @v_calc_allowed) +
            ', Charge: $' + convert(varchar(15), @p_charge_amt) +
            ', Units: ' + convert(varchar(10), @v_adjusted_units) +
            case when @v_mod_adj_factor != 1.0000
                 then ', Mod Adj: ' + convert(varchar(10), @v_mod_adj_factor)
                 else '' end
    end

    return @p_return_code
end
go

/* Grant execute permission */
grant execute on SP_CLAIM_PRICING to hcps_app_role
go

print 'SP_CLAIM_PRICING created successfully.'
go

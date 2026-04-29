/*******************************************************************************
 * HEALTHCARE CLAIMS PROCESSING SYSTEM (HCPS)
 * Database Triggers for Audit Trail
 *
 * This file defines triggers for:
 *   1. TR_CLMHDR_INSERT  - Claim header insert audit
 *   2. TR_CLMHDR_UPDATE  - Claim header update audit (status + amount changes)
 *   3. TR_CLMHDR_DELETE  - Claim header delete audit
 *   4. TR_CLMSTAT_CHANGE - Claim status change tracking
 *   5. TR_PAYMENT_CHANGE - Payment amount change tracking
 *   6. TR_PROVIDER_AUDIT - Provider record change audit
 *
 * Database : HCPS_DB
 * Platform : Sybase ASE
 * Version  : 1.0
 ******************************************************************************/

use HCPS_DB
go


/*******************************************************************************
 * TRIGGER 1: TR_CLMHDR_INSERT
 * Fires on INSERT to CLAIM_HEADER
 * Records new claim creation in the audit log.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_CLMHDR_INSERT' and type = 'TR')
    drop trigger TR_CLMHDR_INSERT
go

create trigger TR_CLMHDR_INSERT
on CLAIM_HEADER
for insert
as
begin
    declare @v_claim_no       varchar(15)
    declare @v_status         char(3)
    declare @v_charge_amt     numeric(13,2)
    declare @v_member_id      varchar(20)
    declare @v_payer_id       varchar(10)
    declare @v_user_id        varchar(30)
    declare @v_timestamp      datetime
    declare @v_program        varchar(30)

    select @v_timestamp = getdate()

    /* Use a cursor to handle multi-row inserts */
    declare ins_cursor cursor for
        select CLM_CLAIM_NO, CLM_STATUS, CLM_TOTAL_CHARGE,
               CLM_MEMBER_ID, CLM_PAYER_ID, CLM_ADD_USER
          from inserted

    open ins_cursor

    fetch ins_cursor into @v_claim_no, @v_status, @v_charge_amt,
        @v_member_id, @v_payer_id, @v_user_id

    while @@sqlstatus = 0
    begin
        /* Audit the claim creation */
        insert into CLAIM_AUDIT_LOG
            (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
             AUDIT_FIELD_NAME, AUDIT_NEW_STATUS, AUDIT_NEW_AMOUNT,
             AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
             AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
             AUDIT_PROGRAM_NAME)
        values
            (@v_claim_no, 'CLAIM_HEADER', 'I',
             'CLM_CLAIM_NO', @v_status, @v_charge_amt,
             'NEWCLM', 'New claim created. Member: ' + @v_member_id +
                        ', Payer: ' + @v_payer_id +
                        ', Charge: $' + convert(varchar(15), @v_charge_amt),
             'TR_CLMHDR_INSERT', isnull(@v_user_id, suser_name()),
             @v_timestamp, 'TR_CLMHDR_INSERT')

        if @@error != 0
        begin
            /* Log trigger error but do not roll back the INSERT */
            raiserror 50001
                'TR_CLMHDR_INSERT: Failed to insert audit record for claim %1!',
                @v_claim_no
        end

        fetch ins_cursor into @v_claim_no, @v_status, @v_charge_amt,
            @v_member_id, @v_payer_id, @v_user_id
    end

    close ins_cursor
    deallocate cursor ins_cursor
end
go

print 'TR_CLMHDR_INSERT created.'
go


/*******************************************************************************
 * TRIGGER 2: TR_CLMHDR_UPDATE
 * Fires on UPDATE to CLAIM_HEADER
 * Tracks all status changes and payment amount changes.
 * Captures old and new values for key fields.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_CLMHDR_UPDATE' and type = 'TR')
    drop trigger TR_CLMHDR_UPDATE
go

create trigger TR_CLMHDR_UPDATE
on CLAIM_HEADER
for update
as
begin
    declare @v_claim_no        varchar(15)
    declare @v_old_status      char(3)
    declare @v_new_status      char(3)
    declare @v_old_net_pay     numeric(13,2)
    declare @v_new_net_pay     numeric(13,2)
    declare @v_old_allowed     numeric(13,2)
    declare @v_new_allowed     numeric(13,2)
    declare @v_old_ded         numeric(13,2)
    declare @v_new_ded         numeric(13,2)
    declare @v_old_deny_rsn    varchar(10)
    declare @v_new_deny_rsn    varchar(10)
    declare @v_old_pend_rsn    varchar(10)
    declare @v_new_pend_rsn    varchar(10)
    declare @v_old_check_no    varchar(20)
    declare @v_new_check_no    varchar(20)
    declare @v_user_id         varchar(30)
    declare @v_timestamp       datetime

    select @v_timestamp = getdate()

    /* Cursor for multi-row updates */
    declare upd_cursor cursor for
        select i.CLM_CLAIM_NO,
               d.CLM_STATUS,           i.CLM_STATUS,
               d.CLM_NET_PAYMENT,      i.CLM_NET_PAYMENT,
               d.CLM_TOTAL_ALLOWED,    i.CLM_TOTAL_ALLOWED,
               d.CLM_TOTAL_DEDUCTIBLE, i.CLM_TOTAL_DEDUCTIBLE,
               d.CLM_DENY_REASON,      i.CLM_DENY_REASON,
               d.CLM_PEND_REASON,      i.CLM_PEND_REASON,
               d.CLM_CHECK_NO,         i.CLM_CHECK_NO,
               isnull(i.CLM_CHG_USER, suser_name())
          from inserted i
               inner join deleted d
                   on i.CLM_CLAIM_NO = d.CLM_CLAIM_NO

    open upd_cursor

    fetch upd_cursor into @v_claim_no,
        @v_old_status, @v_new_status,
        @v_old_net_pay, @v_new_net_pay,
        @v_old_allowed, @v_new_allowed,
        @v_old_ded, @v_new_ded,
        @v_old_deny_rsn, @v_new_deny_rsn,
        @v_old_pend_rsn, @v_new_pend_rsn,
        @v_old_check_no, @v_new_check_no,
        @v_user_id

    while @@sqlstatus = 0
    begin
        /* -----------------------------------------------------------
         * Track STATUS changes
         * -------------------------------------------------------- */
        if @v_old_status != @v_new_status
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_STATUS, AUDIT_NEW_STATUS,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_STATUS', @v_old_status, @v_new_status,
                 @v_old_status, @v_new_status,
                 case @v_new_status
                     when 'RCV' then 'STCHG'
                     when 'EDT' then 'STCHG'
                     when 'PND' then isnull(@v_new_pend_rsn, 'PEND')
                     when 'ADJ' then 'ADJUD'
                     when 'APR' then 'APPVD'
                     when 'DEN' then isnull(@v_new_deny_rsn, 'DENY')
                     when 'PAY' then 'PAID'
                     when 'VDD' then 'VOID'
                     when 'REJ' then 'REJCT'
                     else 'STCHG'
                 end,
                 'Status changed from ' + @v_old_status + ' to ' +
                 @v_new_status +
                 case when @v_new_status = 'DEN' and @v_new_deny_rsn is not null
                      then '. Deny reason: ' + @v_new_deny_rsn
                      else '' end +
                 case when @v_new_status = 'PND' and @v_new_pend_rsn is not null
                      then '. Pend reason: ' + @v_new_pend_rsn
                      else '' end,
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')

            if @@error != 0
            begin
                raiserror 50002
                    'TR_CLMHDR_UPDATE: Failed to audit status change for %1!',
                    @v_claim_no
            end
        end

        /* -----------------------------------------------------------
         * Track NET PAYMENT changes
         * -------------------------------------------------------- */
        if isnull(@v_old_net_pay, 0.00) != isnull(@v_new_net_pay, 0.00)
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_AMOUNT, AUDIT_NEW_AMOUNT,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_NET_PAYMENT',
                 convert(varchar(15), isnull(@v_old_net_pay, 0.00)),
                 convert(varchar(15), isnull(@v_new_net_pay, 0.00)),
                 @v_old_net_pay, @v_new_net_pay,
                 'PYMCHG',
                 'Net payment changed from $' +
                 convert(varchar(15), isnull(@v_old_net_pay, 0.00)) +
                 ' to $' + convert(varchar(15), isnull(@v_new_net_pay, 0.00)),
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')

            if @@error != 0
            begin
                raiserror 50003
                    'TR_CLMHDR_UPDATE: Failed to audit payment change for %1!',
                    @v_claim_no
            end
        end

        /* -----------------------------------------------------------
         * Track ALLOWED AMOUNT changes
         * -------------------------------------------------------- */
        if isnull(@v_old_allowed, 0.00) != isnull(@v_new_allowed, 0.00)
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_AMOUNT, AUDIT_NEW_AMOUNT,
                 AUDIT_REASON_CODE,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_TOTAL_ALLOWED',
                 convert(varchar(15), isnull(@v_old_allowed, 0.00)),
                 convert(varchar(15), isnull(@v_new_allowed, 0.00)),
                 @v_old_allowed, @v_new_allowed,
                 'ALWCHG',
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')
        end

        /* -----------------------------------------------------------
         * Track DEDUCTIBLE changes
         * -------------------------------------------------------- */
        if isnull(@v_old_ded, 0.00) != isnull(@v_new_ded, 0.00)
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_AMOUNT, AUDIT_NEW_AMOUNT,
                 AUDIT_REASON_CODE,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_TOTAL_DEDUCTIBLE',
                 convert(varchar(15), isnull(@v_old_ded, 0.00)),
                 convert(varchar(15), isnull(@v_new_ded, 0.00)),
                 @v_old_ded, @v_new_ded,
                 'DEDCHG',
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')
        end

        /* -----------------------------------------------------------
         * Track CHECK NUMBER assignment
         * -------------------------------------------------------- */
        if isnull(@v_old_check_no, '') != isnull(@v_new_check_no, '')
           and @v_new_check_no is not null
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_CHECK_NO',
                 isnull(@v_old_check_no, 'NULL'),
                 @v_new_check_no,
                 'CHKASN',
                 'Check/EFT assigned: ' + @v_new_check_no,
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')
        end

        /* -----------------------------------------------------------
         * Track DENY REASON changes
         * -------------------------------------------------------- */
        if isnull(@v_old_deny_rsn, '') != isnull(@v_new_deny_rsn, '')
           and @v_new_deny_rsn is not null
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_DENY_REASON',
                 isnull(@v_old_deny_rsn, 'NULL'),
                 @v_new_deny_rsn,
                 @v_new_deny_rsn,
                 'Denial reason set to: ' + @v_new_deny_rsn,
                 'TR_CLMHDR_UPDATE', @v_user_id, @v_timestamp,
                 'TR_CLMHDR_UPDATE')
        end

        fetch upd_cursor into @v_claim_no,
            @v_old_status, @v_new_status,
            @v_old_net_pay, @v_new_net_pay,
            @v_old_allowed, @v_new_allowed,
            @v_old_ded, @v_new_ded,
            @v_old_deny_rsn, @v_new_deny_rsn,
            @v_old_pend_rsn, @v_new_pend_rsn,
            @v_old_check_no, @v_new_check_no,
            @v_user_id
    end

    close upd_cursor
    deallocate cursor upd_cursor
end
go

print 'TR_CLMHDR_UPDATE created.'
go


/*******************************************************************************
 * TRIGGER 3: TR_CLMHDR_DELETE
 * Fires on DELETE from CLAIM_HEADER
 * Records claim deletion in audit log. Physical deletes should be rare
 * in a claims system (prefer void/reversal), so this also raises a warning.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_CLMHDR_DELETE' and type = 'TR')
    drop trigger TR_CLMHDR_DELETE
go

create trigger TR_CLMHDR_DELETE
on CLAIM_HEADER
for delete
as
begin
    declare @v_claim_no       varchar(15)
    declare @v_status         char(3)
    declare @v_charge_amt     numeric(13,2)
    declare @v_net_pay        numeric(13,2)
    declare @v_member_id      varchar(20)
    declare @v_payer_id       varchar(10)
    declare @v_timestamp      datetime

    select @v_timestamp = getdate()

    declare del_cursor cursor for
        select CLM_CLAIM_NO, CLM_STATUS, CLM_TOTAL_CHARGE,
               CLM_NET_PAYMENT, CLM_MEMBER_ID, CLM_PAYER_ID
          from deleted

    open del_cursor

    fetch del_cursor into @v_claim_no, @v_status, @v_charge_amt,
        @v_net_pay, @v_member_id, @v_payer_id

    while @@sqlstatus = 0
    begin
        /* Warn if deleting a paid claim */
        if @v_status in ('PAY', 'APR') and isnull(@v_net_pay, 0.00) > 0.00
        begin
            raiserror 50010
                'WARNING: Deleting paid claim %1! with net payment $%2!. This should be voided instead.',
                @v_claim_no,
                @v_net_pay
        end

        /* Record the deletion in audit log */
        insert into CLAIM_AUDIT_LOG
            (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
             AUDIT_FIELD_NAME, AUDIT_OLD_STATUS, AUDIT_OLD_AMOUNT,
             AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
             AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
             AUDIT_PROGRAM_NAME)
        values
            (@v_claim_no, 'CLAIM_HEADER', 'D',
             'CLM_CLAIM_NO', @v_status, @v_charge_amt,
             'DELETE',
             'CLAIM DELETED. Prior status: ' + @v_status +
             ', Member: ' + @v_member_id +
             ', Payer: ' + @v_payer_id +
             ', Charge: $' + convert(varchar(15), isnull(@v_charge_amt, 0.00)) +
             ', Net Pay: $' + convert(varchar(15), isnull(@v_net_pay, 0.00)),
             'TR_CLMHDR_DELETE', suser_name(), @v_timestamp,
             'TR_CLMHDR_DELETE')

        if @@error != 0
        begin
            raiserror 50011
                'TR_CLMHDR_DELETE: Failed to audit deletion of claim %1!',
                @v_claim_no
        end

        fetch del_cursor into @v_claim_no, @v_status, @v_charge_amt,
            @v_net_pay, @v_member_id, @v_payer_id
    end

    close del_cursor
    deallocate cursor del_cursor
end
go

print 'TR_CLMHDR_DELETE created.'
go


/*******************************************************************************
 * TRIGGER 4: TR_CLMSTAT_CHANGE
 * Fires on UPDATE to CLAIM_HEADER
 * Specialized trigger for claim status workflow validation.
 * Enforces valid status transitions and captures workflow timing.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_CLMSTAT_CHANGE' and type = 'TR')
    drop trigger TR_CLMSTAT_CHANGE
go

create trigger TR_CLMSTAT_CHANGE
on CLAIM_HEADER
for update
as
begin
    /* Only fire when status column is actually updated */
    if not update(CLM_STATUS)
        return

    declare @v_claim_no    varchar(15)
    declare @v_old_status  char(3)
    declare @v_new_status  char(3)
    declare @v_valid       char(1)

    declare stat_cursor cursor for
        select i.CLM_CLAIM_NO, d.CLM_STATUS, i.CLM_STATUS
          from inserted i
               inner join deleted d
                   on i.CLM_CLAIM_NO = d.CLM_CLAIM_NO
         where d.CLM_STATUS != i.CLM_STATUS

    open stat_cursor

    fetch stat_cursor into @v_claim_no, @v_old_status, @v_new_status

    while @@sqlstatus = 0
    begin
        select @v_valid = 'N'

        /* Validate status transition rules */
        /* Valid transitions:
           NEW -> RCV, REJ
           RCV -> EDT, DEN, REJ
           EDT -> PND, ADJ, DEN, REJ
           PND -> EDT, ADJ, DEN
           ADJ -> APR, DEN, PND
           APR -> PAY, VDD
           DEN -> EDT (on appeal)
           PAY -> VDD
           VDD -> (terminal)
        */
        if (@v_old_status = 'NEW' and @v_new_status in ('RCV', 'REJ'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'RCV' and @v_new_status in ('EDT', 'DEN', 'REJ'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'EDT' and @v_new_status in ('PND', 'ADJ', 'DEN', 'REJ'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'PND' and @v_new_status in ('EDT', 'ADJ', 'DEN'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'ADJ' and @v_new_status in ('APR', 'DEN', 'PND'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'APR' and @v_new_status in ('PAY', 'VDD'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'DEN' and @v_new_status in ('EDT'))
            select @v_valid = 'Y'
        else if (@v_old_status = 'PAY' and @v_new_status in ('VDD'))
            select @v_valid = 'Y'

        if @v_valid = 'N'
        begin
            /* Log invalid transition but allow it (some batch processes
               may need to force transitions). A stricter system would
               use rollback trigger here. */
            raiserror 50020
                'WARNING: Invalid status transition %1! -> %2! for claim %3!',
                @v_old_status, @v_new_status, @v_claim_no

            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_STATUS, AUDIT_NEW_STATUS,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'CLAIM_HEADER', 'U',
                 'CLM_STATUS', @v_old_status, @v_new_status,
                 'INVTRAN',
                 'INVALID STATUS TRANSITION: ' + @v_old_status +
                 ' -> ' + @v_new_status,
                 'TR_CLMSTAT_CHANGE', suser_name(), getdate(),
                 'TR_CLMSTAT_CHANGE')
        end

        fetch stat_cursor into @v_claim_no, @v_old_status, @v_new_status
    end

    close stat_cursor
    deallocate cursor stat_cursor
end
go

print 'TR_CLMSTAT_CHANGE created.'
go


/*******************************************************************************
 * TRIGGER 5: TR_PAYMENT_CHANGE
 * Fires on UPDATE to PAYMENT_HISTORY
 * Tracks changes to payment records, especially void and adjustment activity.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_PAYMENT_CHANGE' and type = 'TR')
    drop trigger TR_PAYMENT_CHANGE
go

create trigger TR_PAYMENT_CHANGE
on PAYMENT_HISTORY
for update
as
begin
    declare @v_pymnt_id       numeric(15,0)
    declare @v_claim_no       varchar(15)
    declare @v_old_status     char(2)
    declare @v_new_status     char(2)
    declare @v_old_amount     numeric(13,2)
    declare @v_new_amount     numeric(13,2)
    declare @v_old_check_no   varchar(20)
    declare @v_new_check_no   varchar(20)
    declare @v_user_id        varchar(30)
    declare @v_timestamp      datetime

    select @v_timestamp = getdate()

    declare pay_cursor cursor for
        select i.PYMNT_ID, i.PYMNT_CLAIM_NO,
               d.PYMNT_STATUS, i.PYMNT_STATUS,
               d.PYMNT_AMOUNT, i.PYMNT_AMOUNT,
               d.PYMNT_CHECK_NO, i.PYMNT_CHECK_NO,
               isnull(i.PYMNT_CHG_USER, suser_name())
          from inserted i
               inner join deleted d
                   on i.PYMNT_ID = d.PYMNT_ID

    open pay_cursor

    fetch pay_cursor into @v_pymnt_id, @v_claim_no,
        @v_old_status, @v_new_status,
        @v_old_amount, @v_new_amount,
        @v_old_check_no, @v_new_check_no,
        @v_user_id

    while @@sqlstatus = 0
    begin
        /* Track payment status changes */
        if @v_old_status != @v_new_status
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_AMOUNT, AUDIT_NEW_AMOUNT,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'PAYMENT_HISTORY', 'U',
                 'PYMNT_STATUS', @v_old_status, @v_new_status,
                 @v_old_amount, @v_new_amount,
                 case @v_new_status
                     when 'VO' then 'PYMVOID'
                     when 'ST' then 'PYMSTOP'
                     when 'CL' then 'PYMCLR'
                     else 'PYMCHG'
                 end,
                 'Payment ID ' + convert(varchar(20), @v_pymnt_id) +
                 ' status: ' + @v_old_status + ' -> ' + @v_new_status +
                 ', Amount: $' + convert(varchar(15), @v_new_amount),
                 'TR_PAYMENT_CHANGE', @v_user_id, @v_timestamp,
                 'TR_PAYMENT_CHANGE')
        end

        /* Track payment amount changes */
        if @v_old_amount != @v_new_amount
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_OLD_AMOUNT, AUDIT_NEW_AMOUNT,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_claim_no, 'PAYMENT_HISTORY', 'U',
                 'PYMNT_AMOUNT',
                 convert(varchar(15), @v_old_amount),
                 convert(varchar(15), @v_new_amount),
                 @v_old_amount, @v_new_amount,
                 'PYMADJ',
                 'Payment amount adjusted from $' +
                 convert(varchar(15), @v_old_amount) +
                 ' to $' + convert(varchar(15), @v_new_amount) +
                 ' (Diff: $' +
                 convert(varchar(15), @v_new_amount - @v_old_amount) + ')',
                 'TR_PAYMENT_CHANGE', @v_user_id, @v_timestamp,
                 'TR_PAYMENT_CHANGE')
        end

        fetch pay_cursor into @v_pymnt_id, @v_claim_no,
            @v_old_status, @v_new_status,
            @v_old_amount, @v_new_amount,
            @v_old_check_no, @v_new_check_no,
            @v_user_id
    end

    close pay_cursor
    deallocate cursor pay_cursor
end
go

print 'TR_PAYMENT_CHANGE created.'
go


/*******************************************************************************
 * TRIGGER 6: TR_PROVIDER_AUDIT
 * Fires on INSERT, UPDATE, DELETE to PROVIDER_MASTER
 * Tracks all changes to provider records for credentialing and compliance.
 ******************************************************************************/

if exists (select 1 from sysobjects where name = 'TR_PROVIDER_INS_AUDIT' and type = 'TR')
    drop trigger TR_PROVIDER_INS_AUDIT
go

create trigger TR_PROVIDER_INS_AUDIT
on PROVIDER_MASTER
for insert
as
begin
    declare @v_prov_id    varchar(15)
    declare @v_npi        char(10)
    declare @v_prov_name  varchar(60)
    declare @v_par_status char(1)
    declare @v_timestamp  datetime

    select @v_timestamp = getdate()

    declare pins_cursor cursor for
        select PROV_PROVIDER_ID, PROV_NPI,
               isnull(PROV_ORG_NAME,
                      PROV_LAST_NAME + ', ' + PROV_FIRST_NAME),
               PROV_PAR_STATUS
          from inserted

    open pins_cursor

    fetch pins_cursor into @v_prov_id, @v_npi, @v_prov_name, @v_par_status

    while @@sqlstatus = 0
    begin
        insert into CLAIM_AUDIT_LOG
            (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
             AUDIT_FIELD_NAME, AUDIT_NEW_VALUE,
             AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
             AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
             AUDIT_PROGRAM_NAME)
        values
            (@v_prov_id, 'PROVIDER_MASTER', 'I',
             'PROV_PROVIDER_ID', @v_prov_id,
             'PRVADD',
             'New provider added. NPI: ' + @v_npi +
             ', Name: ' + @v_prov_name +
             ', Par Status: ' + @v_par_status,
             'TR_PROVIDER_INS_AUDIT', suser_name(), @v_timestamp,
             'TR_PROVIDER_INS_AUDIT')

        fetch pins_cursor into @v_prov_id, @v_npi, @v_prov_name, @v_par_status
    end

    close pins_cursor
    deallocate cursor pins_cursor
end
go

print 'TR_PROVIDER_INS_AUDIT created.'
go


/* Provider UPDATE trigger */
if exists (select 1 from sysobjects where name = 'TR_PROVIDER_UPD_AUDIT' and type = 'TR')
    drop trigger TR_PROVIDER_UPD_AUDIT
go

create trigger TR_PROVIDER_UPD_AUDIT
on PROVIDER_MASTER
for update
as
begin
    declare @v_prov_id         varchar(15)
    declare @v_old_status      char(1)
    declare @v_new_status      char(1)
    declare @v_old_par         char(1)
    declare @v_new_par         char(1)
    declare @v_old_npi         char(10)
    declare @v_new_npi         char(10)
    declare @v_old_fee_sched   varchar(15)
    declare @v_new_fee_sched   varchar(15)
    declare @v_old_reimb_pct   numeric(9,4)
    declare @v_new_reimb_pct   numeric(9,4)
    declare @v_user_id         varchar(30)
    declare @v_timestamp       datetime

    select @v_timestamp = getdate()

    declare pupd_cursor cursor for
        select i.PROV_PROVIDER_ID,
               d.PROV_STATUS, i.PROV_STATUS,
               d.PROV_PAR_STATUS, i.PROV_PAR_STATUS,
               d.PROV_NPI, i.PROV_NPI,
               d.PROV_FEE_SCHED_ID, i.PROV_FEE_SCHED_ID,
               d.PROV_REIMB_PCT, i.PROV_REIMB_PCT,
               isnull(i.PROV_CHG_USER, suser_name())
          from inserted i
               inner join deleted d
                   on i.PROV_PROVIDER_ID = d.PROV_PROVIDER_ID

    open pupd_cursor

    fetch pupd_cursor into @v_prov_id,
        @v_old_status, @v_new_status,
        @v_old_par, @v_new_par,
        @v_old_npi, @v_new_npi,
        @v_old_fee_sched, @v_new_fee_sched,
        @v_old_reimb_pct, @v_new_reimb_pct,
        @v_user_id

    while @@sqlstatus = 0
    begin
        /* Track status changes */
        if @v_old_status != @v_new_status
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_prov_id, 'PROVIDER_MASTER', 'U',
                 'PROV_STATUS', @v_old_status, @v_new_status,
                 'PRVSTC',
                 'Provider status changed: ' + @v_old_status +
                 ' -> ' + @v_new_status,
                 'TR_PROVIDER_UPD_AUDIT', @v_user_id, @v_timestamp,
                 'TR_PROVIDER_UPD_AUDIT')
        end

        /* Track par status changes */
        if @v_old_par != @v_new_par
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_prov_id, 'PROVIDER_MASTER', 'U',
                 'PROV_PAR_STATUS', @v_old_par, @v_new_par,
                 'PRVPAR',
                 'Provider par status changed: ' + @v_old_par +
                 ' -> ' + @v_new_par,
                 'TR_PROVIDER_UPD_AUDIT', @v_user_id, @v_timestamp,
                 'TR_PROVIDER_UPD_AUDIT')
        end

        /* Track NPI changes */
        if @v_old_npi != @v_new_npi
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_prov_id, 'PROVIDER_MASTER', 'U',
                 'PROV_NPI', @v_old_npi, @v_new_npi,
                 'PRVNPI',
                 'Provider NPI changed: ' + @v_old_npi +
                 ' -> ' + @v_new_npi,
                 'TR_PROVIDER_UPD_AUDIT', @v_user_id, @v_timestamp,
                 'TR_PROVIDER_UPD_AUDIT')
        end

        /* Track fee schedule changes */
        if isnull(@v_old_fee_sched, '') != isnull(@v_new_fee_sched, '')
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_prov_id, 'PROVIDER_MASTER', 'U',
                 'PROV_FEE_SCHED_ID',
                 isnull(@v_old_fee_sched, 'NULL'),
                 isnull(@v_new_fee_sched, 'NULL'),
                 'PRVFEE',
                 'Provider fee schedule changed: ' +
                 isnull(@v_old_fee_sched, 'NULL') + ' -> ' +
                 isnull(@v_new_fee_sched, 'NULL'),
                 'TR_PROVIDER_UPD_AUDIT', @v_user_id, @v_timestamp,
                 'TR_PROVIDER_UPD_AUDIT')
        end

        /* Track reimbursement percentage changes */
        if isnull(@v_old_reimb_pct, 0) != isnull(@v_new_reimb_pct, 0)
        begin
            insert into CLAIM_AUDIT_LOG
                (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
                 AUDIT_FIELD_NAME, AUDIT_OLD_VALUE, AUDIT_NEW_VALUE,
                 AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
                 AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
                 AUDIT_PROGRAM_NAME)
            values
                (@v_prov_id, 'PROVIDER_MASTER', 'U',
                 'PROV_REIMB_PCT',
                 convert(varchar(15), isnull(@v_old_reimb_pct, 0)),
                 convert(varchar(15), isnull(@v_new_reimb_pct, 0)),
                 'PRVRTE',
                 'Provider reimbursement rate changed: ' +
                 convert(varchar(15), isnull(@v_old_reimb_pct, 0)) +
                 '% -> ' +
                 convert(varchar(15), isnull(@v_new_reimb_pct, 0)) + '%',
                 'TR_PROVIDER_UPD_AUDIT', @v_user_id, @v_timestamp,
                 'TR_PROVIDER_UPD_AUDIT')
        end

        fetch pupd_cursor into @v_prov_id,
            @v_old_status, @v_new_status,
            @v_old_par, @v_new_par,
            @v_old_npi, @v_new_npi,
            @v_old_fee_sched, @v_new_fee_sched,
            @v_old_reimb_pct, @v_new_reimb_pct,
            @v_user_id
    end

    close pupd_cursor
    deallocate cursor pupd_cursor
end
go

print 'TR_PROVIDER_UPD_AUDIT created.'
go


/* Provider DELETE trigger */
if exists (select 1 from sysobjects where name = 'TR_PROVIDER_DEL_AUDIT' and type = 'TR')
    drop trigger TR_PROVIDER_DEL_AUDIT
go

create trigger TR_PROVIDER_DEL_AUDIT
on PROVIDER_MASTER
for delete
as
begin
    declare @v_prov_id    varchar(15)
    declare @v_npi        char(10)
    declare @v_prov_name  varchar(60)
    declare @v_status     char(1)
    declare @v_timestamp  datetime

    select @v_timestamp = getdate()

    declare pdel_cursor cursor for
        select PROV_PROVIDER_ID, PROV_NPI,
               isnull(PROV_ORG_NAME,
                      PROV_LAST_NAME + ', ' + PROV_FIRST_NAME),
               PROV_STATUS
          from deleted

    open pdel_cursor

    fetch pdel_cursor into @v_prov_id, @v_npi, @v_prov_name, @v_status

    while @@sqlstatus = 0
    begin
        insert into CLAIM_AUDIT_LOG
            (AUDIT_CLAIM_NO, AUDIT_TABLE_NAME, AUDIT_ACTION,
             AUDIT_FIELD_NAME, AUDIT_OLD_VALUE,
             AUDIT_REASON_CODE, AUDIT_REASON_TEXT,
             AUDIT_PROCESS_NAME, AUDIT_USER_ID, AUDIT_TIMESTAMP,
             AUDIT_PROGRAM_NAME)
        values
            (@v_prov_id, 'PROVIDER_MASTER', 'D',
             'PROV_PROVIDER_ID', @v_prov_id,
             'PRVDEL',
             'PROVIDER DELETED. NPI: ' + @v_npi +
             ', Name: ' + @v_prov_name +
             ', Prior Status: ' + @v_status,
             'TR_PROVIDER_DEL_AUDIT', suser_name(), @v_timestamp,
             'TR_PROVIDER_DEL_AUDIT')

        fetch pdel_cursor into @v_prov_id, @v_npi, @v_prov_name, @v_status
    end

    close pdel_cursor
    deallocate cursor pdel_cursor
end
go

print 'TR_PROVIDER_DEL_AUDIT created.'
go

print 'All HCPS audit triggers created successfully.'
go

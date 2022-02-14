class zcl_po_features definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_tax_values,
        ebeln       type ebeln, "pedido
        ebelp       type ebelp, "item
        matnr       type matnr, "material
        werks       type werks_d, "centro
        menge       type j_1bnetqty, "quantidade
        meins       type j_1bnetunt, "unidade
        netbgt      type ze_netbudget, "valor de budget
        zgpb        type ze_suppliervalue, "preço negociado pelo fornecedor
        netpr       type j_1bnfpri, "valor unitario liquido sem impostos
        netwr       type j_1bnetval, "valor bruto com impostos
        nfdis       type j_1bnetdis, "desconto
        nficmsdeson type j_1bnfe_nficmsdeson, "icms desonerado
        nffre       type j_1bnffre, "frete
        nfins       type j_1bnfins, "seguro
        nfoth       type j_1bnfoth, "outras despesas
        vicms       type j_1btaxval, "valor icms
        vicmsst     type j_1btaxval, "valor icmsst
        vdifal      type j_1btaxval, "valor difal
        vipi        type j_1btaxval, "valor IPI
        vpis        type j_1btaxval, "valor PIS
        vcofins     type j_1btaxval, "Valor Cofins
        nfnet       type j_1bnfnet, "valor dos produtos
        nftot       type j_1bnftot, "vaor tota nfe
        condpay     type ze_condpay, "codigo e condição de pagamento
        regio       type regio,
        wkurs       type wkurs,
      end of ty_tax_values .
    types:
      begin of ty_approvals,
        ebeln    type ebeln,
        dtstatus type sy-datum,
        status   type char20,
      end of ty_approvals .
    types:
      tt_approvals type standard table of ty_approvals  with non-unique key ebeln .

    methods get_date_approvals
      importing
        !i_ebeln       type ebeln
      returning
        value(re_date) type reajadjmapprovaldate .
    methods get_taxes_value
      importing
        !i_ebeln type ebeln
        !i_ebelp type ebelp optional
      exporting
        !e_taxes type ty_tax_values .
    methods get_last_price_budget
      importing
        !i_matnr        type matnr
        !i_werks        type werks_d
        !i_date         type ccmvalidtodt default sy-datum
      returning
        value(e_budget) type zmmt010 .
    methods set_approval
      importing
        !i_ebeln         type ebeln optional
      exporting
        value(re_return) type tt_approvals .
    methods set_urgency
      importing
        !i_dtcreate      type sy-datum
        !i_dtdelivery    type sy-datum
        !i_deadline      type plifz
      returning
        value(re_return) type char_60 .
protected section.
private section.


ENDCLASS.



CLASS ZCL_PO_FEATURES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PO_FEATURES->GET_DATE_APPROVALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EBELN                        TYPE        EBELN
* | [<-()] RE_DATE                        TYPE        REAJADJMAPPROVALDATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_date_approvals.
* retorna a data de aprovação do pedido de compras
    select single ebeln, aedat ,procstat into @data(ls_ekko) from ekko where ebeln eq @i_ebeln.

    if ls_ekko-procstat eq '05'." possui estratégia e esta aprovado
      select single max( udate ) as udate, utime
        into @data(ls_docmodif)
        from zddl_docsmodif
        where objectclas = 'EINKBELEG'
        and objectid = @i_ebeln
        and tabname = 'EKKO'
        and fname = 'PROCSTAT'
        and value_new = '05'
        group by utime.
      if ls_docmodif is not initial.
        re_date = ls_docmodif-udate.
      endif.
    elseif ls_ekko-procstat = '02'. "não possui estratégia
      re_date = ls_ekko-aedat.
    endif.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PO_FEATURES->GET_LAST_PRICE_BUDGET
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MATNR                        TYPE        MATNR
* | [--->] I_WERKS                        TYPE        WERKS_D
* | [--->] I_DATE                         TYPE        CCMVALIDTODT (default =SY-DATUM)
* | [<-()] E_BUDGET                       TYPE        ZMMT010
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_last_price_budget.

    "Seleciona a condição de preço do budget com base na data do pedido de compras
    select * from zmmt010
      into table @data(lt_budget)
      where matnr = @i_matnr
      and werks = @i_werks
      and datum <= @i_date
      order by datum descending , uzeit descending.


    try.
        e_budget =  lt_budget[ 1 ] .
      catch cx_sy_itab_line_not_found.

    endtry.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PO_FEATURES->GET_TAXES_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EBELN                        TYPE        EBELN
* | [--->] I_EBELP                        TYPE        EBELP(optional)
* | [<---] E_TAXES                        TYPE        TY_TAX_VALUES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_taxes_value.

    data: hwert       type bset-fwste,
          komk        type komk,
          tkomv       type table of komv,
          tkomvd      type table of komvd,
          taxsheet    type t005-kalsm,
          wa_taxcom   type taxcom,
          "xkomv     TYPE TABLE OF komv,
          t_xkomv     type table of komv,
          lv_lifnr    type lfa1-lifnr,
          lv_conv_amt type cms_dte_conv_amt,
          lv_rate     type ukurs_curr.


* Cabeçalho Pedido de Compras
    select single *
      into @data(ls_ekko)
      from ekko
      where ebeln = @i_ebeln.
    if sy-subrc ne 0.
      "MESSAGE e019(06) WITH i_ebeln RAISING pedido_inexistente.
    endif.

* Item Pedido de Compras
    select single *
      from ekpo
      into @data(ls_ekpo)
      where ebeln = @i_ebeln
        and ebelp = @i_ebelp.

    if sy-subrc ne 0.
      "MESSAGE e471(06) RAISING pedido_inexistente.
    endif.

    if ls_ekko-llief is not initial.
      lv_lifnr = ls_ekko-llief. "Fornecedor Mercadoria(parceiro)
    else.
      lv_lifnr = ls_ekko-lifnr. "Fornecedor do Pedido
    endif.

* texto da condição da pagamento
    Select single zterm, text1  into @data(ls_t052U)
       from t052u
       where zterm = @ls_ekko-zterm
          and spras = @sy-langu.


* Obter dados Fornecedor
    select single *
      into @data(ls_lfa1)
      from lfa1
     where lifnr = @lv_lifnr.
    if sy-subrc ne 0.
      "MESSAGE e025(06) RAISING fornecedor_inexistente.
    endif.

    komk-mandt = ls_ekko-mandt.
    if ls_ekko-kalsm ne space.
      komk-kalsm = ls_ekko-kalsm.
    else.
      komk-kalsm = 'RM0000'.
    endif.
    komk-kappl = 'M'.
    komk-waerk = ls_ekko-waers.
    komk-knumv = ls_ekko-knumv.
    komk-bukrs = ls_ekko-bukrs.

* Seta parametros de cabeçalho para a memória
    call function 'RV_PRICE_PRINT_HEAD'
      exporting
        comm_head_i = komk
        language    = ls_ekko-spras
      importing
        comm_head_e = komk
      tables
        tkomv       = tkomv
        tkomvd      = tkomvd.

    call function 'FIND_TAX_SPREADSHEET'
      exporting
        buchungskreis = ls_ekko-bukrs
      importing
        schema        = taxsheet
      exceptions
        not_found     = 1
        others        = 2.

    clear wa_taxcom.
    wa_taxcom-bukrs = ls_ekpo-bukrs.
    wa_taxcom-budat = ls_ekko-bedat.
    wa_taxcom-bldat = ls_ekko-bedat.
    wa_taxcom-waers = ls_ekko-waers.
    wa_taxcom-hwaer = ls_ekko-waers.
    wa_taxcom-kposn = ls_ekpo-ebelp.
    wa_taxcom-mwskz = ls_ekpo-mwskz.
    wa_taxcom-shkzg = 'H'.
    wa_taxcom-lifnr = lv_lifnr.
    wa_taxcom-ekorg = ls_ekko-ekorg.
    wa_taxcom-matnr = ls_ekpo-matnr.
    wa_taxcom-werks = ls_ekpo-werks.
    wa_taxcom-matkl = ls_ekpo-matkl.
    wa_taxcom-meins = ls_ekpo-meins.
    if ls_ekko-bstyp = 'F'.
      wa_taxcom-mglme = ls_ekpo-menge.
    else.
      wa_taxcom-mglme = ls_ekpo-ktmng.
    endif.
    if wa_taxcom-mglme eq 0.
      wa_taxcom-mglme = 1000.
    endif.
    wa_taxcom-mtart = ls_ekpo-mtart.
    wa_taxcom-xmwst = 'X'.
    wa_taxcom-txjcd = ls_ekpo-txjcd.
    wa_taxcom-ebeln = ls_ekpo-ebeln.
    wa_taxcom-ebelp = ls_ekpo-ebelp.
    wa_taxcom-land1 = ls_ekko-lands. "BR
    wa_taxcom-llief = ls_ekko-llief.
    wa_taxcom-bwtar = ls_ekpo-bwtar.
    if ls_ekko-bstyp eq 'F'.
      wa_taxcom-wrbtr = ls_ekpo-netwr.
    else.
      wa_taxcom-wrbtr = ls_ekpo-zwert.
    endif.

    call function 'J_1B_NF_PO_DISCOUNTS'
      exporting
        i_kalsm = ls_ekko-kalsm
        i_ekpo  = ls_ekpo
      importing
        e_ekpo  = ls_ekpo
      tables
        i_konv  = tkomv
      exceptions
        others  = 1.

    perform j_1b_save_tax_fields in program sapmm06e
         using ls_ekko
               ls_ekpo
               ls_lfa1.

    "CLEAR hwert.
    call function 'CALCULATE_TAX_ITEM'
      exporting
        display_only        = 'X'
        i_taxcom            = wa_taxcom
      importing
        e_taxcom            = wa_taxcom
        nav_anteil          = hwert
      tables
        t_xkomv             = t_xkomv
      exceptions
        mwskz_not_defined   = 1
        mwskz_not_found     = 2
        mwskz_not_valid     = 3
        steuerbetrag_falsch = 4
        country_not_found   = 5
        others              = 6.

* Limpa tabelas
    call function 'J_1B_REFRESH_TAX_FIELDS'.
    call function 'J_1B_REFRESH_PO_CONDS'.
    call function 'J_1B_REFRESH_NF_TAX_INFO'.
    call function 'REFRESH_TAX_TABLES'.


    "Seleciona condições especificas
    select * from prcd_elements
      into table @data(lt_elements)
      where knumv = @ls_ekko-knumv and kposn eq @ls_ekpo-ebelp.


    clear e_taxes.

    e_taxes-ebeln = ls_ekko-ebeln.
    e_taxes-ebelp = ls_ekpo-ebelp.
    e_taxes-regio = ls_lfa1-regio.
    e_taxes-wkurs = ls_ekko-wkurs.
    if ls_ekko-bstyp = 'F'.
      e_taxes-menge = ls_ekpo-menge.
    else.
      e_taxes-menge = ls_ekpo-ktmng.
    endif.
    e_taxes-meins = ls_ekpo-meins.
    e_taxes-matnr = ls_ekpo-matnr.
    e_taxes-werks = ls_ekpo-werks.


    try.
        e_taxes-vicms =  t_xkomv[ kschl = 'BX13' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vicms = 0.
    endtry.

    try.
        e_taxes-vipi =  t_xkomv[ kschl = 'BX23' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vipi = 0.
    endtry.

    try.
        e_taxes-vicmsst =  t_xkomv[ kschl = 'BX41' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vicmsst = 0.
    endtry.

    try.
        e_taxes-vcofins =  t_xkomv[ kschl = 'BX72' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vcofins = 0 .
    endtry.

    try.
        e_taxes-vpis =  t_xkomv[ kschl = 'BX82' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vpis = 0.
    endtry.

    try.
        e_taxes-vdifal =  t_xkomv[ kschl = 'ICOP' ]-kwert.
      catch cx_sy_itab_line_not_found.
        e_taxes-vdifal = 0.
    endtry.

    try.
        e_taxes-nficmsdeson =  0.
      catch cx_sy_itab_line_not_found.
        e_taxes-nficmsdeson = 0.
    endtry.

    "Valor de budget
    

     "Valor sem impostos do budget
    try.
        e_taxes-netbgt = ( lt_elements[ kschl = 'ZBGT' ]-kwert  ) .
      catch cx_sy_itab_line_not_found.
        e_taxes-netbgt = 0.
    endtry.

     "Valor liquido sem imposto
     try.
        e_taxes-netpr = ( lt_elements[ kschl = 'ZSIM' ]-kwert  ) .
      catch cx_sy_itab_line_not_found.

        e_taxes-netpr = ( ( ls_ekpo-netwr  - ( e_taxes-vpis + e_taxes-vcofins ) )  )  .
    endtry.

    "Valor do fornecedor
    try.
        e_taxes-zgpb = ( lt_elements[ kschl = 'ZGPB' ]-kwert  ) .
      catch cx_sy_itab_line_not_found.
        e_taxes-zgpb = 0.
    endtry.

    "Valor Desconto
    try.
        e_taxes-nfdis = ( lt_elements[ kschl = 'RB00' ]-kwert  ) .
      catch cx_sy_itab_line_not_found.
        e_taxes-nfdis = 0.
    endtry.

    "Valor frete
    try.
        e_taxes-nffre = ( lt_elements[ kschl = 'FRB1' ]-kwert  ) .
      catch cx_sy_itab_line_not_found.
        e_taxes-nffre = 0.
    endtry.


    "Valor liquido dos impostos( excluindo PIS/COFINS
*
*
*    try.
*        e_taxes-netpr =  t_xkomv[ kschl = 'BASB' ]-kwert - ( e_taxes-vpis + e_taxes-vcofins ).
*      catch cx_sy_itab_line_not_found.
*        e_taxes-netpr = ( ( ls_ekpo-netwr  - ( e_taxes-vpis + e_taxes-vcofins ) )  )  .
*    endtry.

    e_taxes-netwr =  ls_ekpo-netwr + e_taxes-vicms.

    e_taxes-nfins = 0.
    e_taxes-nfoth = 0.

    "total dos produtos
    e_taxes-nfnet = e_taxes-netwr +  e_taxes-nfdis .
    e_taxes-nftot = e_taxes-netwr + e_taxes-vipi + e_taxes-vicmsst + e_taxes-nffre + e_taxes-nfins + e_taxes-nfoth.


    e_taxes-condpay = |{ ls_t052u-zterm } - { ls_t052u-text1 }|.




  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PO_FEATURES->SET_APPROVAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_EBELN                        TYPE        EBELN(optional)
* | [<---] RE_RETURN                      TYPE        TT_APPROVALS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_approval.
    constants: c_relgroup type char2 value '01',
               c_relcode  type char2 value '00'.

    data: lt_headers       type table of bapiekkol,
          lt_items         type table of bapiekpoc,
          lt_return        type table of bapireturn,
          rg_ebeln         type range of ebeln,
          rg_knumv         type range of knumv,
          lt_for_approvals type table of ebeln,

          vlr_zbgt         type  vfprc_element_value,
          vlr_zsim         type vfprc_element_value,
          vlr_total_zbgt   type  vfprc_element_value,
          vlr_total_zsim   type vfprc_element_value.



    "Verifica se pedido possui pedencia de aprovação no nivel 00 - pricing
    call function 'BAPI_PO_GETITEMSREL'
      exporting
        rel_group         = c_relgroup
        rel_code          = c_relcode
        items_for_release = 'X'
      tables
        po_headers        = lt_headers
        po_items          = lt_items.
    "return            = lt_return.



    if lt_headers is not initial.


      "Seleciona condições de preços dos itens do pedido, condição preço liquido e preço de budget
      if i_ebeln is initial.
        rg_ebeln = value #( for ls_ebeln in lt_headers ( option = 'EQ' sign = 'I' low = ls_ebeln ) ).
      else.
        rg_ebeln = value #( ( option = 'EQ' sign = 'I' low = i_ebeln ) ).
      endif.

      select ekko~ebeln, elements~knumv, elements~kposn, elements~kschl, elements~kwert
       from (  ekko as ekko inner join prcd_elements as elements on elements~knumv = ekko~knumv )
       into table @data(lt_elements)
       where elements~kappl = 'M'
       and elements~kschl in ( 'ZSIM', 'ZBGT' )
       and ekko~ebeln in @rg_ebeln.


      loop at lt_headers into data(ls_header) where po_number in rg_ebeln.

        loop at lt_items  assigning field-symbol(<ls_item>) where po_number eq ls_header-po_number.
          "valida para o item se há algum registro cujo o preço de compra liquido de impostos é maior que o budget.
          try.
              vlr_zsim = lt_elements[ ebeln = <ls_item>-po_number kposn = <ls_item>-po_item kschl = 'ZSIM' ]-kwert.
            catch cx_sy_itab_line_not_found.
              vlr_zsim = 0 .
          endtry.

          try.
              vlr_zbgt = lt_elements[ ebeln = <ls_item>-po_number kposn = <ls_item>-po_item kschl = 'ZBGT' ]-kwert.
            catch cx_sy_itab_line_not_found.
              vlr_zbgt = 0 .
          endtry.

          "valida se o valor liquido maior que o budget
          if vlr_zsim > vlr_zbgt.
            <ls_item>-acknowl_no = 'NOK'.
          else.
            <ls_item>-acknowl_no = 'OK'.
          endif.

        endloop.

        "Se para o items existir ao menos 1 com diferença de valor, nao aprova o pedido.
        if line_exists( lt_items[ po_number = ls_header-po_number acknowl_no = 'NOK' ] ).
          CONTINUE.
          "exit. "pedido deve ser aprovado manualmente
        else.
          "pedido aprovado automaticamente.

          call function 'BAPI_PO_RELEASE'
            exporting
              purchaseorder          = ls_header-po_number
              po_rel_code            = c_relcode
            tables
              return                 = lt_return
            exceptions
              authority_check_fail   = 1
              document_not_found     = 2
              enqueue_fail           = 3
              prerequisite_fail      = 4
              release_already_posted = 5
              responsibility_fail    = 6
              others                 = 7.
          if sy-subrc <> 0.
          "  append lt_return to re_return.

          endif.
          "re_return = VALUE #( BASE re_return( ebeln = ls_header-po_number dtstatus = sy-datum status = 'Aprovado' ) ).
          append value #( ebeln = ls_header-po_number dtstatus = sy-datum status = 'Aprovado' ) to re_return.
        endif.
      endloop.
     " re_return = lt_return.
    endif.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_PO_FEATURES->SET_URGENCY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DTCREATE                     TYPE        SY-DATUM
* | [--->] I_DTDELIVERY                   TYPE        SY-DATUM
* | [--->] I_DEADLINE                     TYPE        PLIFZ
* | [<-()] RE_RETURN                      TYPE        CHAR_60
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SET_URGENCY.
   data: ls_delivery_plan  TYPE sy-datum.
    re_return = ''.

    "data planejada =  data da criação/solicitação + dias prazo previsto
    ls_delivery_plan = i_dtcreate + i_deadline.

    "Verifica se a data de entrega é maior que a data planejada original
    if ls_delivery_plan > i_dtdelivery  .
      data(ls_days) =  ls_delivery_plan - i_dtdelivery .
      re_return = |remessa adiantada em { ls_days } dia(s), prazo previsto { ls_delivery_plan DATE = USER } |.
    endif.


  endmethod.
ENDCLASS.

;----------------------------------------------------------------------------------------
pro Convert2img_event, Event

  COMPILE_OPT IDL2
  
  UNAME = Widget_Info(Event.id, /UNAME)
  
  Widget_Control, event.top, Get_UValue=pstate, /No_Copy
  Widget_Control, event.id,  Get_UValue=uvalue
  
  case uvalue of
    'hdf_BUTTON' :  begin
      filename = dialog_pickfile(/read, filter=['*.hdf'],title = 'Please choose file')
      if filename eq '' then return
      Widget_CONTROL, (*pstate).hdf_TEXT, set_value = filename
      (*pstate).inhdffile = filename
      Widget_CONTROL, Event.top, set_uvalue = pstate      
    end
    'Inundation'   : (*pstate).Inundation=~(*pstate).Inundation
    'QC'     : (*pstate).QC=~(*pstate).QC
    'projection'  : (*pstate).projection=event.index    
    'img_BUTTON' :  begin
      imgfile = dialog_pickfile(/read, filter=['*.hdf'],title = 'Please enter output file name')
      if imgfile eq '' then return else imgfile = imgfile+'.img'
      Widget_CONTROL, (*pstate).img_TEXT, set_value = imgfile
      (*pstate).ouimgfile = imgfile
      Widget_CONTROL, Event.top, set_uvalue = pstate
     end
    'APPLY_BUTTON' :  begin
      if strlen((*pstate).inhdffile) eq 0 then begin
        Result = DIALOG_MESSAGE('Please input the inundation file' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif
      if strlen((*pstate).ouimgfile) eq 0 then begin
        Result = DIALOG_MESSAGE('Please input the product name' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif
        sd_names=[]
        if (*pstate).Inundation eq 1 then sd_names=[sd_names,'Inundation']
        if (*pstate).QC eq 1 then sd_names=[sd_names,'QC']
        num = n_elements(sd_names)
      if num eq 0 then begin
        Result = DIALOG_MESSAGE('No data product selected' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif       
        for i=0,num-1 do begin
          filepath_arr = strsplit((*pstate).ouimgfile,'.',/extract)
          ouimgfile = filepath_arr[0]+'_'+sd_names[i]+'.img'
          HDFtoImg,(*pstate).inhdffile,sd_names[i],ouimgfile,reprojected=~(*pstate).projection
        endfor     
      
      Result = DIALOG_MESSAGE('Convertion sucess!' , /CENTER , /INFORMATION, TITLE='Convert2img')
      ;Widget_CONTROL, Event.top, /DESTROY
      
    end
  endcase
  Widget_Control, event.top, Set_UValue=pstate, /No_Copy
end



PRO Convert2img, event

  compile_opt idl2
  
  ; Event handler to select changes to the data object.
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  infoBase = widget_base(title='Convert Inundation',UNAME='Convert_Inundation', /base_align_center)
  
  textBase   = widget_base(infoBase, /column)
  
  rowBase = widget_base(textBase,/column,frame=1)
  xBase   = widget_base(rowBase, /row)
  hdf_TEXT = CW_FIELD(xBase, value=' ',title='Input hdf file:',/STRING, xsize=20)
  hdf_BUTTON = widget_button(xbase, value='Browse', uvalue='hdf_BUTTON',xsize=75)
  yBase   = widget_base(rowBase,Scr_XSize=200, /NonExclusive,/row)
  button_Inundation = Widget_Button(yBase, Value='Inundation',uvalue='Inundation')
  button_QC = Widget_Button(yBase, Value='QC',uvalue='QC')
  Widget_Control, button_Inundation, Set_Button=1
  zBase   = widget_base(rowBase, /row)
  projectionlist=['geographic','Sinusoidal']
  dropFields = widget_droplist(zBase, VALUE=projectionlist,title='projection: ',uvalue='projection')
  aBase   = widget_base(rowBase, /row)
  img_TEXT = CW_FIELD(aBase, value=' ',title='Output img file:',/STRING, xsize=20)
  img_BUTTON = widget_button(abase, value='Browse', uvalue='img_BUTTON',xsize=75)
  
  rowBase2 = widget_base(textBase, row=1, /align_center)
  APPLY_BUTTON = widget_button(rowBase2, value='Apply', uvalue='APPLY_BUTTON',xsize=75)

    
  state  =  {hdf_TEXT:hdf_TEXT,img_TEXT:img_TEXT, $
    inhdffile:'', ouimgfile:'',$
    Inundation:1,QC:0,$
    projection:0}
    
  pstate = ptr_new(state, /NO_COPY)
  
  widget_control, infoBase, /REALIZE
  widget_control, infoBase, set_uvalue=pstate, /realize
  ; Redraw the graphic.
  Widget_Control, event.top, Set_UValue=info, /No_Copy
  xmanager, 'Convert2img', infoBase, cleanup='Convert2img_cleanup'
  
END
;-------------------------------------------------------------------------------------------------------------

pro Convert2img_cleanup, infoBase

  compile_opt idl2, logical_predicate
  
  widget_control, infoBase, get_uvalue=pstate
  
  ptr_free, pstate
  
END
;----------------------------------------------------------------------------------------

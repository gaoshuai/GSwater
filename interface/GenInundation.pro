;----------------------------------------------------------------------------------------
pro GenInundation_event, Event

  COMPILE_OPT IDL2
  

  Widget_Control, event.top, Get_UValue=pstate, /No_Copy
  Widget_Control, event.id,  Get_UValue=uvalue
  
  case uvalue of
    'mod_BUTTON' :  begin
      filespath = dialog_pickfile(title = 'Choose directory containning MOD09 files',/DIRECTORY)
      if filespath eq '' then return
      Widget_CONTROL, (*pstate).mod_TEXT, set_value = filespath
      (*pstate).inmodfile = filespath
      Widget_CONTROL, Event.top, set_uvalue = pstate
      
    end
    'hdf_BUTTON' :  begin
      Inunfile = dialog_pickfile(/read, filter=['*.hdf'],title = 'Please enter output file name')
      if Inunfile eq '' then return else Inunfile = Inunfile+'.hdf'
      Widget_CONTROL, (*pstate).hdf_TEXT, set_value = Inunfile
      (*pstate).ouhdffile = Inunfile
      Widget_CONTROL, Event.top, set_uvalue = pstate
      
    end
    'APPLY_BUTTON' :  begin
      if strlen((*pstate).ouhdffile) eq 0 then begin
        Result = DIALOG_MESSAGE('Please input the modisA1 files path' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif
      if strlen((*pstate).inmodfile) eq 0 then begin
        Result = DIALOG_MESSAGE('Please input the product name' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif
      mod09a1files = FILE_SEARCH((*pstate).inmodfile,'*.hdf',COUNT = hdfcnt)
      if hdfcnt eq 0 then begin
        Result = DIALOG_MESSAGE('There is no MOD09A1 file in the path' , /CENTER , /INFORMATION, TITLE='Warning')
        break
      endif
      
      envi_report_init, ['It will take several minites'], title='generating Inundation product', base=base
      envi_report_inc, base, hdfcnt
      envi_report_stat, base, 1, hdfcnt
      
      Inundation_HDF,MOD09A1files,(*pstate).ouhdffile
      
      envi_report_init, base=base, /finish
      
      Result = DIALOG_MESSAGE('Inundation generated!' , /CENTER , /INFORMATION, TITLE='GenInundation')
      ;Widget_CONTROL, Event.top, /DESTROY
      
    end
  endcase
Widget_Control, event.top, Set_UValue=pstate, /No_Copy  
end



PRO GenInundation, event

  compile_opt idl2
  
  ; Event handler to select changes to the data object.
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  infoBase = widget_base(title='Generate Inundation',UNAME='Generate_Inundation', /base_align_center)
  
  textBase   = widget_base(infoBase, /column)
  rowBase = widget_base(textBase,/column,frame=1)
  xBase   = widget_base(rowBase, /row)
  mod_TEXT = CW_FIELD(xBase, value=' ',title='Input Files Path:',/STRING, xsize=20)
  mod_BUTTON = widget_button(xbase, value='Browse', uvalue='mod_BUTTON',xsize=75)
  yBase   = widget_base(rowBase, /row)
  hdf_TEXT = CW_FIELD(yBase, value=' ',title='Output HDF file:',/STRING, xsize=20)
  hdf_BUTTON = widget_button(ybase, value='Browse', uvalue='hdf_BUTTON',xsize=75)
  zbase = widget_base(textBase, row=1, /align_center)
  APPLY_BUTTON = widget_button(zbase, value='Apply', uvalue='APPLY_BUTTON',xsize=75)
  
  state  =  {mod_TEXT:mod_TEXT, hdf_TEXT:hdf_TEXT,$
    inmodfile:'', ouhdffile:''}
  pstate = ptr_new(state, /NO_COPY)
  
  widget_control, infoBase, /REALIZE
  widget_control, infoBase, set_uvalue=pstate, /realize
  ; Redraw the graphic.
  Widget_Control, event.top, Set_UValue=info, /No_Copy
  xmanager, 'GenInundation', infoBase, cleanup='GenInundation_cleanup'
  
END
;-------------------------------------------------------------------------------------------------------------

pro GenInundation_cleanup, infoBase

  compile_opt idl2, logical_predicate
  
  widget_control, infoBase, get_uvalue=pstate
  
  ptr_free, pstate
  
END
;----------------------------------------------------------------------------------------

;+
; NAME:
;    addchar
;
; PURPOSE:
;    Return the string with she supplement character(s) if the length of sting is less than the length
;
; INPUTS:
;    str     string
;    supp    supplement character(s)
;    length  the length of the new string;    
;
; OUTPUTS:
;    new string
;
; EXAMPLE:
;   print,addchar('15','0',3)
;
; AUTHOR:
;
;       Shuai Gao
;       Institute of Remote Sensing and Digital Earth
;       Chinese Academy of Sciences
;       C300,3 Datun Road,Chaoyang District
;       Beijing 100101,P.R. China
;       EMAIL:gaoshuai@live.com

;###########################################################################

FUNCTION addchar,str,supp,length
  compile_opt idl2, logical_predicate
  
  orilen = strlen(str)
  if orilen lt length then begin
    for i=0,length-orilen-1 do begin
      str=supp+str
    endfor
  endif
  
  return,str
  
end

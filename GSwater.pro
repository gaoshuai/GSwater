;+
; NAME:
;
;       LidarTools_BCAL
;
; PURPOSE:
;
;       The purpose of this program is create menu system in ENVI
;
; PRODUCTS:
;
;
; AUTHOR:
;
;       Rupesh Shrestha
;       Boise Center Aerospace Laboratory
;       Idaho State University
;       322 E. Front St., Ste. 240
;       Boise, ID  83702
;       http://bcal.geology.isu.edu/
;
; DEPENDENCIES:
;
;
; MODIFICATION HISTORY:
;
;       Written by Rupesh Shrestha, April 2010.
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright @ 2010 Rupesh Shrestha, Idaho State University.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################

; Begin main program

pro GSwater_define_buttons, buttonInfo

  compile_opt idl2
  
  ; Main 'BCAL LiDAR' Menu
  
  envi_define_menu_button, buttonInfo, value='GS water', /menu, ref_value='Topographic', $
    /sibling, position='after'
           
  ; Main Menu
    
  envi_define_menu_button, buttonInfo, value='Inundation Product', event_pro='GenInundation', $
    position=0, ref_value='GS water', uvalue='Inundation'
    
  envi_define_menu_button, buttonInfo, value='HDFtoImg', event_pro='Convert2img', $
    position=1, ref_value='GS water',  uvalue='hdftoimg'
    
   
end

; main program

pro GSwater, event

end
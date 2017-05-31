;+
; NAME:
;    batchmosaic
;
; PURPOSE:
;    Batch mosaic the img files with envi format
;
; INPUTS:
;    startyear  
;    endyear
;
; OUTPUTS:
;    the whole mosaic file
;
; EXAMPLE:
;    batchmosaic,2000,2014
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
pro batchmosaic,startyear,endyear
  compile_opt idl2, logical_predicate

  path = 'G:\inun\'  
  dirs = ['h25v03','h25v04','h26v03','h26v04','h27v04','h27v05']  
  numtiles = n_elements(dirs)
  
  years = fix(endyear)-fix(startyear)+1
  year = indgen(years)+startyear
  
  for i = 0, years -1 do begin
    files =[]
    for j =0, numtiles -1 do begin
      file= FILE_SEARCH(path+'inundation_'+dirs[j],'inundation_'+dirs[j]+'_'+strcompress(year[i],/remove)+'_Inun'+'*.img',COUNT = hdfcnt)
      if file ne '' then files = [files,file]
    endfor
    mosaicfile = path+'mosaic_'+strcompress(year[i],/remove)+'.img'
    indband = 1
    processmosaic, path, files, mosaicfile, indband
    ;print,i+2000,mosaicfile
  endfor
  
end


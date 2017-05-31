;+
; NAME:
;    CalInun_years,calculate the inundation erery year
;    HDF2Img_years, Convert the hdf format file to envi img format file every year
;    CalInun_days,,calculate the inundation erery year for a date range
;
; PURPOSE:
;    Batch process the modis mod09a1 file including calcute the inundation and convert it to imgfile
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
pro CalInun_years,startyear,endyear
  compile_opt idl2, logical_predicate
  
  path = 'F:\MOD09A1\'
  dirs = ['h24v05','h25v03','h25v04','h25v05','h25v06','h26v03','h26v04'$
         ,'h26v05','h26v06','h27v04','h27v05','h27v06','h28v05','h28v06','h28v07','h29v06']     
  nums = n_elements(dirs)   
      
  years = fix(endyear)-fix(startyear)+1        
  year = indgen(years)+startyear   
  
  for i=0, nums-1 do begin
    for j =0, years -1 do begin
      files = FILE_SEARCH(path+'MOD09A1_'+dirs[i],'MOD09A1.A'+strcompress(year[j],/remove)+'*'+dirs[i]+'*.hdf',COUNT = hdfcnt)
      if hdfcnt gt 0 then begin
         Inunfile = path+'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf'
;        Inunfile = path+'inundation_'+dirs[i]+'\'+'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf'
        Inundation_HDF,files,Inunfile
      endif
    end
  endfor
end

;#############################################################################################
pro HDF2Img_years,startyear,endyear
compile_opt idl2, logical_predicate

path = 'F:\inundation\'
dirs = ['h23v04','h23v05','h24v04','h24v05','h25v03','h25v04','h25v05','h25v06','h26v03','h26v04'$
       ,'h26v05','h26v06','h27v04','h27v05','h27v06','h28v05','h28v06','h28v07','h29v06']
numtiles = n_elements(dirs)

years = fix(endyear)-fix(startyear)+1
year = indgen(years)+startyear

for i=0, numtiles-1 do begin
  for j =0, years -1 do begin
    file = FILE_SEARCH(path+'inundation_'+dirs[i],'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf',COUNT = hdfcnt)
;    file = FILE_SEARCH(path,'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf',COUNT = hdfcnt)
    if hdfcnt eq 1 then begin
      filepath_arr = strsplit(file[0],'.',/extract)
      Img_Inundation = filepath_arr[0]+'_'+'Inun'+'.img'
      Img_QC = filepath_arr[0]+'_'+'QC'+'.img'
      HDFtoImg,file[0],'Inundation',Img_Inundation,/reprojected
      HDFtoImg,file[0],'QC',Img_QC,/reprojected
    endif
  end
endfor
end
;#############################################################################################
pro CalInun_days,startday,endday,startyear,endyear
  compile_opt idl2, logical_predicate

  path = 'G:\MOD09A1\'  
  dirs = ['h24v05','h25v03','h25v04','h25v05','h25v06','h26v03','h26v04'$
    ,'h26v05','h26v06','h27v04','h27v05','h27v06','h28v05','h28v06','h28v07','h29v06']
  numtiles = n_elements(dirs)

  years = fix(endyear)-fix(startyear)+1
  year = indgen(years)+startyear
  
  for i=0, numtiles-1 do begin
    for j =0, years -1 do begin
      files =[]
      for k = fix(startday),fix(endday),8 do begin
        file= FILE_SEARCH(path+'MOD09A1_'+dirs[i],'MOD09A1.A'+strcompress(year[j],/remove)+addchar(strcompress(k,/remove),'0',3)+'*'+dirs[i]+'*.hdf',COUNT = hdfcnt)
        if file ne '' then files = [files,file]
      endfor
      Inunfile = path+'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf'
      print,j+2000,n_elements(files)
      Inundation_HDF,files,Inunfile
    endfor
   endfor 
 
end

;;#############################################################################################
;pro convertdata
;  compile_opt idl2, logical_predicate
;  
;  ;dirs = ['h23v04','h23v05','h24v04','h24v05','h25v03','h25v04','h25v05','h25v06','h26v03','h26v04'$
;  ;       ,'h26v05','h26v06','h27v04','h27v05','h27v06','h28v05','h28v06','h28v07','h29v06']
;  dirs = ['h25v05']
;  nums = n_elements(dirs)
;  years = 16
;  year = indgen(years)+2000
;  path = 'G:\MOD09A1\'
;  for i=0, nums-1 do begin
;    for j =0, years -1 do begin
;      file = FILE_SEARCH(path+'inundation_'+dirs[i]+'_24','inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf',COUNT = hdfcnt)
;      ;    file = FILE_SEARCH(path,'inundation_'+dirs[i]+'_'+strcompress(year[j],/remove)+'.hdf',COUNT = hdfcnt)
;      if hdfcnt eq 1 then begin
;        filepath_arr = strsplit(file[0],'.',/extract)
;        Img_Inundation = filepath_arr[0]+'_'+'Inun'+'.img'
;        Img_QC = filepath_arr[0]+'_'+'QC'+'.img'
;        HDFtoImg,file[0],'Inundation',Img_Inundation,/reprojected
;        HDFtoImg,file[0],'QC',Img_QC,/reprojected
;      endif
;    end
;  endfor
;end
;
;;######################################################################################

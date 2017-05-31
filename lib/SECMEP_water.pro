;+
; NAME:
;    NDWI
;
; PURPOSE:
;    Return NDWI and quality control (QC) for the MOD09A1 product with HDF format
;
; INPUTS:
;    MOD09A1file     Name of MOD09A1 product with HDF format
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    StructMeta      If set, the meta info including the coordanate info will stored in the sturucture.
;
; OUTPUTS:
;    NDWI_Value the data array of NDWI
;    NDWI_QC the data array of NDWI QC file
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    None.
;
; RESTRICTIONS:
;    Requires IDL 5.0 or higher (square bracket array syntax).
;
; EXAMPLE:
;   NDWI,'MOD09A1.A2000097.h25v03.005.2008199004752.hdf',NDWI_Value,NDWI_QC,StructMeta = StructMeta
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
PRO NDWI,MOD09A1file,NDWI_Value,NDWI_QC,StructMeta = StructMeta

 compile_opt idl2, logical_predicate
 
 if (n_params() ne 3) then message, 'Usage: HDF_NDWI,MOD09A1file,NDWI_Value,NDWI_QC,StructMeta = StructMeta'
 hdfid = hdf_sd_start(MOD09A1file)
 HDF_SD_VARREAD, hdfid, 'sur_refl_b04', sur_refl_b04
 HDF_SD_VARREAD, hdfid, 'sur_refl_b06', sur_refl_b06
 HDF_SD_VARREAD, hdfid, 'sur_refl_qc_500m', sur_refl_qc_500m
 dims = size(sur_refl_b04)
 
 StructMeta = HDF_SD_ATTINFO(HDFID, '', 'StructMetadata.0', /GLOBAL)
 _FillValue_04=HDF_SD_ATTINFO(hdfid, 'sur_refl_b04', '_FillValue')
 _FillValue_06=HDF_SD_ATTINFO(hdfid, 'sur_refl_b06', '_FillValue')
 scale_factor_04=HDF_SD_ATTINFO(hdfid, 'sur_refl_b04', 'scale_factor')
 scale_factor_06=HDF_SD_ATTINFO(hdfid, 'sur_refl_b06', 'scale_factor')
 add_offset_04=HDF_SD_ATTINFO(hdfid, 'sur_refl_b04', 'add_offset')
 add_offset_06=HDF_SD_ATTINFO(hdfid, 'sur_refl_b06', 'add_offset')
 
 b04=sur_refl_b04*scale_factor_04.data[0]+add_offset_04.data[0]
 b06=sur_refl_b06*scale_factor_06.data[0]+add_offset_06.data[0]
 NDWI = make_array(dims[1],dims[2],value =0,/float)
 ind_nozero = where((b04+b06) ne 0,cnt)
 NDWI[ind_nozero]=(b04[ind_nozero]-b06[ind_nozero])/(b04[ind_nozero]+b06[ind_nozero])
 
 ;NDWI file: Water 1;Non-water 0;Fillvalue -2
 NDWI_Value = make_array(dims[1],dims[2],value =0,/integer)
 ind_ndwigt0 = where(NDWI gt 0,cnt_gt0)
 ind_ndwilt0 = where(NDWI lt 0,cnt_lt0)
 ind_b04fill = where(sur_refl_b04 eq _FillValue_04.data[0],cnt_Fill04)
 ind_b06fill = where(sur_refl_b06 eq _FillValue_06.data[0],cnt_Fill06)
 NDWI_Value[ind_ndwigt0] = 1
 NDWI_Value[ind_ndwilt0] = 0
 NDWI_Value[ind_b04fill] = -1
 NDWI_Value[ind_b06fill] = -1
 
 ;QC file: good 0; not-good 1 
 ;22-25 band 6 and 14-17 band4
 NDWI_QC = make_array(dims[1],dims[2],value =0,/integer)
 qc_b06=(sur_refl_qc_500m mod 2^26)/2^22 
 qc_b04=(sur_refl_qc_500m mod 2^17)/2^14
 ind_b04_qcfail = where(qc_b04 ne 0,cnt_b04_qcfail)
 ind_b06_qcfail = where(qc_b06 ne 0,cnt_b06_qcfail)
 NDWI_QC[ind_b04_qcfail] = 1
 NDWI_QC[ind_b06_qcfail] = 1
 hdf_sd_end,hdfid
 
end 
;+
; NAME:
;    Inundation_HDF
;
; PURPOSE:
;    Return inundation file with HDF format
;
; INPUTS:
;    MOD09A1files  Name list of MOD09A1 product with HDF format
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    None.
;
; OUTPUTS:
;    Inunfile: inundation file name with HDF format
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    None.
;
; RESTRICTIONS:
;    Requires IDL 5.0 or higher (square bracket array syntax).
;
; EXAMPLE:
;   files = FILE_SEARCH('E:\huawei\MODIS-2014','*.hdf',COUNT = hdfcnt)
;   Inundation_HDF,files,'E:\huawei\inundation_2014.hdf'
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
pro Inundation_HDF,MOD09A1files,Inunfile

 compile_opt idl2, logical_predicate
 if (n_params() ne 2) then message, 'Usage: Inundation_Fre,MOD09A1files,Inunfile'
 nums = n_elements(MOD09A1files)
 ;files = FILE_SEARCH('E:\huawei\MODIS-2014','*.hdf',COUNT = hdfcnt)
 NDWI,MOD09A1files[0],NDWI_Value,NDWI_QC,StructMeta = StructMeta
 dims = size(NDWI_Value)
 NUM = make_array(dims[1],dims[2],value =nums,/integer)
 NUM = (NDWI_Value < 0) + NUM
 DATA = NDWI_Value > 0
 QC = NDWI_QC
 for i=1,nums-1 do begin
  NDWI,MOD09A1files[i],NDWI_Value,NDWI_QC
  NUM = (NDWI_Value < 0) + NUM
  DATA = DATA + (NDWI_Value > 0)
  QC = QC+ NDWI_QC
 endfor
 QC_flag = make_array(dims[1],dims[2],value =0,/integer)
 ind_nozero = where(NUM ne 0,cnt)
 QC_flag[ind_nozero] = fix((NUM[ind_nozero]-QC[ind_nozero])/float(NUM[ind_nozero])*10000); scale_factor = 0.0001
 ;creat water hdf files
 hdfid = hdf_sd_start(Inunfile,/create)

 inuid = hdf_sd_create(hdfid,'Inundation',[dims[1],dims[2]],/int)
 hdf_sd_adddata,inuid,DATA
 hdf_sd_attrset,inuid,'long_name','Inundation Frequency Index'
 hdf_sd_attrset,inuid,'scale_factor',1, /INT
 hdf_sd_attrset,inuid,'add_offset',0, /INT
 hdf_sd_endaccess,inuid
; 
 qcid = hdf_sd_create(hdfid,'QC',[dims[1],dims[2]],/int)
 hdf_sd_adddata,qcid,QC_flag
 hdf_sd_attrset,qcid,'long_name','Quality_control_flags'
 hdf_sd_attrset,qcid,'scale_factor',0.0001, /DOUBLE
 hdf_sd_attrset,qcid,'add_offset',0, /DOUBLE
 hdf_sd_endaccess,qcid
; 
 hdf_sd_attrset,hdfid,'creation_data',systime()
 hdf_sd_attrset,hdfid,'StructMeta.0',StructMeta.data
 hdf_sd_attrset,hdfid,'Numbers',nums, /INT
 hdf_sd_end,hdfid

end
;+
; NAME:
;    HDFtoImg,Inunfile,sd_name,imgfile,reprojected=reprojected
;
; PURPOSE:
;    Convert the inundation file with HDF format to that with ENVI STANDARD img format
;
; INPUTS:
;    Inunfile the inunfile with hdf format
;    sd_name the scitific dataset name : Inundation or QC
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    reprojected: if set,the reproject file with geographic projection will be generated or the grid file will be produced.
;
; OUTPUTS:
;    imgfile: standard ENVI image file with '.img' 
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    None.
;
; RESTRICTIONS:
;    Requires IDL 5.0 or higher (square bracket array syntax).
;
; EXAMPLE:
;   HDF2Img,'inundation_2014.hdf','Inundation','inundation_2014_2.img',/reprojected
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
PRO HDFtoImg,Inunfile,sd_name,imgfile,reprojected=reprojected

  COMPILE_OPT IDL2
  envi, /restore_base_save_files
  envi_batch_init
  
  ;filepath_arr = strsplit(Inunfile,'.',/extract)
  ;Img_name = filepath_arr[0]+'_'+sd_name+'.img'
  if sd_name eq 'Inundation' then data_type=2 else data_type=5
  
  hdfid = hdf_sd_start(Inunfile)
  StructMeta = HDF_SD_ATTINFO(HDFID, '', 'StructMeta.0', /GLOBAL)
  ;set the map info and projection info
  strtmp = Strsplit(StructMeta.data,/extract,COUNT=count)
  for i=0,count-1 do begin
    str = Strsplit(strtmp[i], '=', /extract)
    if str[0] eq 'XDim' then ns = fix(str[1])
    if str[0] eq 'YDim' then nl = fix(str[1])
    if str[0] eq 'UpperLeftPointMtrs' then ul = double(strsplit(replace_string(replace_string(str[1],'(',''),')',''),',',/extract))
    if str[0] eq 'LowerRightMtrs' then lr = double(strsplit(replace_string(replace_string(str[1],'(',''),')',''),',',/extract))
    if str[0] eq 'ProjParams' then ProjParams = double(strsplit(replace_string(replace_string(str[1],'(',''),')',''),',',/extract))
  endfor
  Inun_PE_COORD_SYS_STR = 'PROJCS["Sinusoidal",GEOGCS["GCS_Unknown",DATUM["D_Unknown",SPHEROID["S_Unknown",6371007.18,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Sinusoidal"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",0.0],UNIT["Meter",1.0]]'
  Inunproj= ENVI_PROJ_CREATE(type= 16,pe_coord_sys_str=Inun_PE_COORD_SYS_STR)
  Inunproj.PARAMS = ProjParams
  Inunmc=[0,0,ul]
  x_pixelsize = (lr[0]-ul[0])/ns
  y_pixelsize = (ul[1]-lr[1])/nl
  InunmapInfo  = envi_map_info_create(proj=Inunproj, ps=[x_pixelsize,y_pixelsize], mc=Inunmc)
  ;read data and write data
  HDF_SD_VARREAD, hdfid, sd_name, data
  scale_factor=HDF_SD_ATTINFO(hdfid, sd_name, 'scale_factor')
  add_offset=HDF_SD_ATTINFO(hdfid, sd_name, 'add_offset')
  data =data*scale_factor.data[0]+add_offset.data[0]
  
  InunmapInfo  = envi_map_info_create(proj=Inunproj, ps=[x_pixelsize,y_pixelsize], mc=Inunmc)
  
  if ~keyword_set(reprojected) then begin
    envi_write_envi_file, data, r_fid=tempId, nb=1, nl=nl, ns=ns, map_info=InunmapInfo, $
      /no_open, data_type=data_type, interleave=0, OUT_NAME=imgfile
  endif else begin
    dumpfile = 'tmp_'+STRTRIM(round(randomu(s, 1) * 10e8),1)+'.img'
    envi_write_envi_file, data, r_fid=tempId, nb=1, nl=nl, ns=ns, map_info=InunmapInfo, $
      /no_open, data_type=data_type, interleave=0, OUT_NAME=dumpfile
    envi_open_file, dumpfile , r_fid=fid
    if (fid eq -1) then message, 'fid eq -1'
    envi_file_query, fid, dims=dims, nb=nb
    o_proj = ENVI_PROJ_CREATE(/geographic)
    o_pixel_size = [0.008370d,0.008370d]
    ;Choosing Triangulation with Nearest Neighbor.
    warp_method = 2
    resampling=0
    
    envi_convert_file_map_projection, fid=fid, $
      pos=[0], dims=dims, o_proj=o_proj, $
      o_pixel_size = o_pixel_size,$
      out_name=imgfile, warp_method=warp_method, $
      resampling=resampling, background=0
    ENVI_FILE_MNG,id = fid,/remove
    dumppath=strsplit(dumpfile,'.',/extract)
    dumpfilehead = dumppath[0]+'.hdr'
    file_delete,dumpfilehead,dumpfile
  endelse  
end

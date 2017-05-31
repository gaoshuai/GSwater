;My mosaic
;+
; :Description:
;    'ENVI, /RESTROE_BASE_SAVE_FILES' and 'ENVI_BATCH_INIT, LOG_FILE = 'bathc.log'' must be 
;    executed first before run this procedure. And then follows 'ENVI_BATCH_EXIT'.
;    
; :Author: Dabin
; Reference: http://blog.sina.com.cn/s/blog_4edba2dd01000bjm.html
;-
;envi,/RESTORE_BASE_SAVE_FILES
;ENVI_BATCH_INIT
PRO processmosaic, dirname, filenames, outfname, bands
    COMPILE_OPT IDL2
    
    ;filenames = FILE_SEARCH(dirname + prefix + sufix, COUNT = filecount)
    in_fid = LONARR(SIZE(filenames, /N_ELEMENTS))
    pos = LONARR(bands, SIZE(filenames, /N_ELEMENTS))
    dims = LONARR(5, SIZE(filenames, /N_ELEMENTS))
    use_see_through = INTARR(SIZE(filenames, /N_ELEMENTS))
    see_through_val = INTARR(SIZE(filenames, /N_ELEMENTS))
    
    FOR i = 0, SIZE(filenames, /N_ELEMENTS) - 1 DO BEGIN
        ENVI_OPEN_FILE, filenames[i], R_FID = fid
        IF (fid[0] EQ -1) THEN RETURN
        in_fid[i] = fid[0]
        pos[*, i] = LINDGEN(bands)
        ENVI_FILE_QUERY, fid, NL = nl, NS = ns, NB = nb, DATA_TYPE = dtype
        dims[*, i] = [-1, 0, ns - 1, 0, nl - 1]
        use_see_through[i] = 1
        see_through_val[i] = 0.0

    ENDFOR
        
    ;---------------------------------------------
    ; Get corner coordinates
    ;---------------------------------------------
    ul_x = DBLARR(SIZE(filenames, /N_ELEMENTS))
    ul_y = DBLARR(SIZE(filenames, /N_ELEMENTS))
    lr_x = DBLARR(SIZE(filenames, /N_ELEMENTS))
    lr_y = DBLARR(SIZE(filenames, /N_ELEMENTS))
    
    FOR j = 0, SIZE(filenames, /N_ELEMENTS) - 1 DO BEGIN
        ENVI_CONVERT_FILE_COORDINATES, in_fid[j], [0, dims[2, j]], [0, dims[4, j]], $
                                           XMAP, YMAP, /TO_MAP
        ENVI_CONVERT_FILE_COORDINATES, in_fid[0], xf, yf, XMAP, YMAP
        
        ul_x[j] = xf[0]
        ul_y[j] = yf[0]
        lr_x[j] = xf[1]
        lr_y[j] = yf[1]
    ENDFOR
    
    ulx = MIN(ul_x)
    uly = MIN(ul_y)
    
    x0 = LONG(FLOOR(ul_x - ulx))
    y0 = LONG(FLOOR(ul_y - uly))
    
    lrx = MAX(lr_x)
    lry = MAX(lr_y)
    
    ns_out = LONG(CEIL(lrx) - FLOOR(ulx)) + 1
    nl_out = LONG(CEIL(lry) - FLOOR(uly)) + 1
    
    ENVI_CONVERT_FILE_COORDINATES, in_fid[0], ulx, uly, XMAP, YMAP, /TO_MAP
    mc = [0, 0, XMAP, YMAP]
    proj = ENVI_GET_PROJECTION(FID = in_fid[0], PIXEL_SIZE = pixel_size)
    map_info = ENVI_MAP_INFO_CREATE(MC = mc, PS = pixel_size, PROJ = proj)
    
    xsize = ns_out * pixel_size[0]
    ysize = nl_out * pixel_size[1]
    
    ENVI_DOIT, 'mosaic_doit', FID = in_fid, OUT_NAME = outfname, DIMS = dims, $
                BACKGROUND = 0,POS = pos, USE_SEE_THROUGH = use_see_through, $
                SEE_THROUGH_VAL = see_through_val, OUT_DT = dtype, $
                /GEOREF, MAP_INFO = map_info, PIXEL_SIZE = pixel_size, $
                XSIZE = xsize , YSIZE = ysize, X0 = x0, Y0 = y0
                
    
   ;******REMOVE FIDS AND DELETE INPUTS
    for i=0,SIZE(filenames, /N_ELEMENTS) - 1 do begin
    envi_file_mng,id=in_fid[i],/remove,/delete
    endfor

    
    END
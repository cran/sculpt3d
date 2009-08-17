sculpt3d <-
function(x, y = NULL, z = NULL, col = 'black', labels = NULL, radius = NULL, type = 'p', alpha=NULL, callback=NULL, size = NULL ){
	
	.local$callback = callback
	
	if (exists('gWidget', .local)){ # get rid of any old interface/plots
		try(.local$gWidget$destroy(),silent=TRUE)
		try(rgl.set(.local$rgl.cur),silent=TRUE)
		try(rgl.close(),silent=TRUE)
		# (rgl::rgl.cur() != .local$rgl.cur)
	}
	
	
	.local$size <- size
	.local$radius <- radius
	.local$type <- type
	coords <- xyz.coords(x,y,z)
	.local$x <- coords$x
	.local$y <- coords$y
	.local$z <- coords$z
	if (is.null(labels)) labels = 1:length(.local$x)
	.local$show_labels = FALSE
	.local$selected_color = 'red'
	
	if (length(labels) != length(.local$x)) {
		.local$labels <- rep(labels, length(.local$x), length.out = length(.local$x))
	} else {.local$labels <- col}
	
	
	if (length(col) != length(.local$x)) {
		.local$base_colors <- rep(col, length(.local$x), length.out = length(.local$x))
	} else {.local$base_colors <- col}

	if (length(radius) != length(.local$x) && !is.null(radius)) {
		.local$radius <- rep(radius, length(.local$x),length.out = length(.local$x))
	} else {.local$radius <- radius}

	if (is.null(labels)){
		.local$show_labels = FALSE
	} else {		
		if (length(labels) != length(coords$x)) {
			stop('"labels" argument must equal the total number of datapoints.')
		} else {
			.local$labels <- labels
		}
	}
	
	if (is.null(coords$xlab)){
		.local$xlab <- 'x'; .local$ylab <- 'y'; .local$zlab <- 'z'
	} else {
		.local$xlab = coords$xlab; .local$ylab= coords$ylab; .local$zlab = coords$zlab;
	}

	if (!is.null(alpha)){
		if(length(alpha) ==1) .local$alpha = rep(alpha,length(.local$x))
		else{
			if(length(alpha) != length(.local$x)) stop('"alpha" length must be equal to one or length of "x".')
			.local$alpha = alpha
		}
	}
	
	.local$current <- rep(TRUE,length(.local$x))
	.local$selected <- rep(FALSE, length(.local$x))
	
	.local$rgl.cur <- open3d()
	.filterPlot3d(.local$current) # call plot3d with the current points and filter
	result <- try(etc <- file.path(.path.package(package="sculpt3d")[1], "etc"), silent=TRUE)

	if (inherits(result, "try-error"))
	    gxml = gladeXMLNew("rglToolbar.glade")
	  else
	    gxml = gladeXMLNew(file.path(etc,"rglToolbar.glade"))

	.local$gWidget <-  gxml$getWidget(name='rglToolbar')
	.local$gWidget$setKeepAbove(TRUE)
	.local$gWidget$Show()
		

	.local$select3d_button <- gxml$getWidget('Select3d')	
	
	label_button = gxml$getWidget('Label')
	
	if(is.null(.local$labels)) label_button$sensitive = FALSE
	
	color_button =  gxml$getWidget('SelectColor')
	.local$color_swatch <- gxml$getWidget('ColorSwatch')

	.local$color_swatch$modifyBg("normal",as.GdkColor(.local$selected_color))

	valid_mouse_modes = c("none", "trackball", "xAxis", "yAxis", "zAxis", "polar", "zoom","fov")
	init_modes = r3dDefaults$mouseMode

	.local$crop_button <- gxml$getWidget('Crop'); 
	.local$delete_button <- gxml$getWidget('Delete'); 

	.local$crop_button$sensitive = FALSE
	.local$delete_button$sensitive = FALSE
	
	
	
	option_button <- gxml$getWidget('Options')
	reset_button <- gxml$getWidget('Reset')

	left_click = gxml$getWidget('LeftClick') 
	left_click$setActive(which(init_modes[1] == valid_mouse_modes)-1)

	middle_click = gxml$getWidget('MiddleClick') 
	middle_click$setActive(which(init_modes[2] == valid_mouse_modes)-1)
	right_click = gxml$getWidget('RightClick') 
	
	right_click$setActive(which(init_modes[3] == valid_mouse_modes)-1)
	# right_click$setFocusOnClick(TRUE)

	x = gSignalConnect(left_click, 'changed', .changeClick)
	x = gSignalConnect(middle_click, 'changed', .changeClick)
	x = gSignalConnect(right_click, 'changed', .changeClick)

	x = gSignalConnect(color_button,'clicked', .colorSet)
	x = gSignalConnect(.local$select3d_button,'toggled', .selectData)
	x = gSignalConnect(label_button,'toggled', .toggleLabel)
	x = gSignalConnect(.local$crop_button, 'clicked', .cropData)
	x = gSignalConnect(.local$delete_button, 'clicked', .deleteData)
	x = gSignalConnect(reset_button,'clicked', .resetData)
}


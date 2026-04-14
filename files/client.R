library(httr2)
library(jpeg)

render.matrix = function(M,palette=c('▓','∏','░',':',' ')) cat('\r', c('.','\t',palette)[t(cbind(-2,M,matrix(-1,nrow(M),20)))+3], sep='')
quantize.image = function(img,size=200,shades=4) floor(shades * (0.2126 * img[,,1] + 0.7152 * img[,,2] + 0.0722 * img[,,3]))[seq(1,nrow(img),by=3*nrow(img)/size),seq(1,ncol(img),by=ncol(img)/size)]

stream.client = function(url="http://127.0.0.1:8080",image_size=200,start_at = 30, framerate = 10, video_split = 120){

	frame = start_at
	cat('Waiting for stream to start...\n')
	
	while(TRUE){
		t_start = as.double(Sys.time())
		tryCatch({
			#attempt to get new frame, don't update current frame if that fails
			paste0(url,'/',frame,'.jpeg') |> #request url
				request() |> req_perform() |> resp_body_raw() |> #get raw response
				readJPEG() |> quantize.image(image_size) |> render.matrix() #parse and draw image

			cat(frame,'      ',sep='')
			
			frame = frame + 1
		},error = function(cnd){NULL}) #ignore errors
	
		buffer = as.double(Sys.time()) - t_start #time it took to query and draw
		if (buffer < 1/framerate) Sys.sleep(1/framerate - buffer) #wait til next frame
		else frame = frame + 1 #took too long, skip next frame to get back ahead
		
		if (frame > framerate * (video_split - 5)) frame = start_at #loop when you get to end
	}
}
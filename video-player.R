library(av)
library(jpeg)

video_path = 'XcQ.mp4'

#i HATE the look of this page with all the documentation here. Maybe write an Rmd instead? I'd really like to stress the simplicity of this stuff
#maybe have a bare this.R and an RMD article explaining it

#rmd format:
# what the package does + demo
# all the functions by name and  brief desc
# section for each function w/ code and how it works and examples



#' Render Matrix to Console
#' 
#' Converts and prints a matrix as unicode characters specified by `palette`, clearing the previous frame.
#' 
#' The function adds newlines to the matrix, serializes it, and uses it to key `palette` to convert the integers into characters. This character string is then printed with `cat()`, overwriting any previous cat output with a carriage return (`\r`).
#' 
#' @param M Integer matrix with values corresponding to the character in `palette` to render. (indexed by 0)
#' @param palette Vector of characters to render for values in `M` of 0, 1, 2, etc. Defaults to 1-bit black/white.
#' @section Newlines:
#' Several `\t`'s are used in place of the actual newline character `\n`. Due to how tab characters are rendered, this produces the same behavior as a newline while keeping the entire `cat()` output in a single string so that it can be overwritten by `\r`.
render.matrix = function(M,palette=c('▓',' ')) cat('\r', c('\t',palette)[t(cbind(M,matrix(-1,nrow(M),10)))+2], sep='')

#' Downscale and Convert Image for Rendering
#' 
#' Downsamples, grayscales, and quantizes an image to produce a renderable bitmap matrix. The resultant matrix has 2:1 detail resolution (i.e. twice as much resolution horizontally than vertical) to match how characters are rendered to the console, and it is composed of integers corresponding to the pixel's shade. 
#' 
#' Performs simple luma grayscaling and nearest-neighbor downsampling. Grayscale colors are quantized linearly to match the desired number of `shades`.
#' 
#' @param img RGB image array as produced by `jpeg::readJPEG()`.
#' @param size Width to downscale image to.
#' @param shades Shades of gray to quantize the image to. 2 = black/white, 3 = black/gray/white, etc.
render.image = function(img,size=128,shades=2){ #size = horizontal resolution
	by = ncol(img)/size
	
	#nearest-neighbor downsample, with double horizontal resolution
	img = img[seq(1,nrow(img),by*2),seq(1,ncol(img),by),]
	
	#luma greyscale
	img = 0.2126 * img[,,1] + 
				0.7152 * img[,,2] + 
				0.0722 * img[,,3]
	
	#filter
	floor(img * shades)
}

#' Load Video File for Playing
#'
#' Downscales input video and extracts frames to be used in `play.video()`.
#' 
#' `base::readJPEG()` is costly at higher resolutions, so it's worth formally downscaling (i.e. with traditional video tools) the video before attempting to play it. Be sure not to downscale further than you intend to render.
#'
#' @param path Path to video file. I think this works for any video format.
#' @param size Width to downscale video to. Doesn't have to be exact; anything below 400 will produce nice output video.
load.video = function(path,size=128) { #converts to low-res 25fps
	cat('Downscaling...\n')
	info = av_media_info(path)$video
	
	scale = c(info$width,info$height)
	scale = scale * (size/scale[1]) #match horizontal scale
	scale = floor(scale/2)*2 #make even
	
	temp = tempdir()
	temp = paste0(temp,'downsampled.mp4')
	av_encode_video(path,output = temp,framerate = info$framerate,vfilter = paste0("scale=",scale[1],':',scale[2])) #downscale video
	cat('Extracting frames...\n')
	av_video_images(temp,fps=25)
}

play.video = function(video_data,size=128,palette=c('▓','∏','░',':',' ')){
	target_framerate = 25 #should match actual video
	
	frame = 1
	while (frame < length(video_data)){
		t = as.double(Sys.time()) #get time before rendering frame
		
		#render frame
		render.matrix(render.image(readJPEG(video_data[frame]),size,shades=length(palette)),palette=palette)
		
		dur = as.double(Sys.time()) - t #how long that took to render
		
		dur_frames = dur/(1/target_framerate) #how long that took in frames
		
		frame = frame + ceiling(dur_frames) #advance to next frame
		
		#buffer
		buff = (dur_frames - floor(dur_frames)) * 1/target_framerate
		
		Sys.sleep(1/target_framerate - buff) #sleep to the start of the next frame
	}	
}


#framerate of original video: av_media_info(video_path)$video
vid = load.video(video_path)
play.video(vid)

#the framerate will mainly depend on how quickly readJPEG can load each frame, o< the actual video resolution





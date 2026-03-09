library(av)
library(jpeg)

video_path = 'XcQ.mp4'

render.matrix = function(M,palette=c('▓',' ')) cat('\r', c('\t',palette)[t(cbind(M,matrix(-1,nrow(M),10)))+2], sep='')

render.image = function(img,shades=2) floor(shades * (0.2126 * img[,,1] + 0.7152 * img[,,2] + 0.0722 * img[,,3]))

load.video = function(path,size=128,framerate=25) {
	cat('Downscaling...\n')
	info = av_media_info(path)$video
	
	scale = c(info$width,info$height)
	scale = scale * (size/scale[1]) #match horizontal scale
	scale[2] = scale[2]/2 #halve vertical scale
	scale = floor(scale/2)*2 #make even
	
	temp = tempdir()
	temp = paste0(temp,'downsampled.mp4')
	av_encode_video(path,output = temp,framerate = framerate,vfilter = paste0("scale=",scale[1],':',scale[2])) #downscale video
	cat('Extracting frames...\n')
	av_video_images(temp,fps=framerate)
}

play.video = function(video_data,target_framerate=25,palette=c('▓','∏','░',':',' ')){
	frame = 1
	while (frame < length(video_data)){
		t = as.double(Sys.time()) #get time before rendering frame
		
		#render frame
		render.matrix(render.image(readJPEG(video_data[frame]),shades=length(palette)),palette=palette)
		
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





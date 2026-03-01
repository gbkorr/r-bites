



render.matrix = function(M,palette=c('╬',' ')) cat('\r', c('\t',palette)[t(cbind(M,matrix(-1,nrow(M),10)))+2], sep='')


library(av)
library(jpeg)
viddata = av_video_images('/Users/gabrielbroussardkorr/Desktop/music/Youtube Archive/hacona/Spring Rain ｜ 春雨.mp4')

render.image = function(img,size=64,nv=2){
	#pad size to account for cropping later
	size = size + 4
	
	by = ncol(img)/size
	
	#nearest-neighbor downsample
	img = img[seq(1,nrow(img),by*2),seq(1,ncol(img),by),]
	
	#luma greyscale
	img = 0.2126 * img[,,1] + 0.7152 * img[,,2] + 0.0722 * img[,,3]
	
	#filter
	img = floor(img * nv)
	
	return(img)
}

play.video = function(video_files,size=64,framerate=30,palette=c('╬',' ')){
	for (frame in video_files){
		render.matrix(render.image(readJPEG(frame),size,nv=length(palette)),palette=palette)
		Sys.sleep(1/framerate)
	}
}

play.video = function(video_files,size=128,target_framerate=30,palette=c('▓','∏','░',':',' ')){
	for (frame in 1:length(video_files)){
		t = as.double(Sys.time())
		
		render.matrix(render.image(readJPEG(video_files[frame]),size,nv=length(palette)),palette=palette)
		
		dur = as.double(Sys.time()) - t #how long that took to render
		
		if (dur < 1/target_framerate) Sys.sleep(1/target_framerate - dur)
	}	
}

pal = function(str) strsplit(str,'')[[1]]

#testpalette = pal('╬: ')
#testpalette = pal('@#%*+=-:.') #https://alexharri.com/blog/ascii-rendering
testpalette = pal('▓∏░: ')

play.video(viddata,palette=testpalette)


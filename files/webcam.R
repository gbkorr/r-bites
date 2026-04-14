library(httpuv)

#outputs filename when a new one is created
watch.directory = function(directory){
	files = list.files(directory)
	while(TRUE){
		newfile = setdiff(list.files(directory),files) #new file not in `files` 
		
		if (length(newfile)) return(newfile)
		
		Sys.sleep(0.1) #refresh rate isn't important because we delay afterwards anyway
	}
}
#we can't pick up mid-stream— we have to wait and start as soon as the file starts

stream.webcam = function(directory,url="http://127.0.0.1:8080", framerate = 10, video_split = 120){ #seconds between video splits
tryCatch(finally=stopAllServers(),error=stopAllServers(),expr={ #catch exits to stop server
	cat('Waiting for stream to start...\n')
	
	#watch directory for the stream to start
	stream = paste0(directory,watch.directory(directory))
	
	file.remove(list.files(tempdir(), full.names = TRUE)) #wipe tempdir
	output = tempdir()	#files storing the most recent frames
	#filenames: '/1.jpeg', etc.
	
	#server that serves back whatever you ask it from output dir
	#the client will decide how to access the files
	stopAllServers()
	host = gsub(':.*','',gsub('h.*/','',url)) #remove http and port
	port = gsub('.*:','',url) |> as.integer()
	server = runStaticServer(dir = output, host = host, port = port, background=TRUE, browse=FALSE)
	
	while (TRUE) {
		Sys.sleep(1) #wait for the file to be properly initialized
		
		#save frames as images 
		system(
			timeout = video_split - 5, #stop after almost 60 seconds (video should split)
			command = paste(
				'tail -c +1 -F',stream,'|',
				'ffmpeg',	#search from end, just enough to load fully
				'-i -', #input the result of the pipe
				'-r', framerate, #10 fps
				'-q:v 2', #high quality
				paste0(output,'/%d.jpeg')
		))
		
		#OBS should have cut and made a new file, so delete the old one and switch:
		file.remove(stream)
		file.remove(list.files(tempdir(), full.names = TRUE)) #wipe image frames
		
		cat('\nSplitting video...\n')
		stream = paste0(directory,watch.directory(directory)) #wait for the new file again
	}
	
	#hack to prevent ffmpeg from quitting on EOF while still reading at max speed
	#https://ffmpeg.org/pipermail/ffmpeg-user/2016-July/032874.html
})}




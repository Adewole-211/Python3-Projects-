from pytube import YouTube

link =  input("link: ")
yt = YouTube(link)

video = yt.streams.get_highest_resolution()
video.download("/Users/Adewole/Videos/Youtube Vids")
print("Done!")
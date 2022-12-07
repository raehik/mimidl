# mimicle service investigations
## Downloading data from mimicle
### Terminology
  * **mimicle API:** the collection of "internal" HTTP endpoints that the
    website app (and almost certainly the smartphone apps) use to communicate
    with mimicle's servers
  * **NID:** a type of identifier string used by mimicle. The most common NID is
    an album NID, which is the identifying string in the URL for an album page
    e.g. `VmztIQgt` https://mimicle.com/album/VmztIQgt

### Audio
#### TODO
  * Audio streaming setup is similar to
    https://en.wikipedia.org/wiki/HTTP_Live_Streaming
    * Unlike the above, mimicle gives each quality its own playlist

#### Full abstract procedure for obtaining ergonomic audio data from mimicle
Steps prefixed with `API:` mean "this involves making such requests to the
mimicle API".

Given a list of NIDs of albums you wish to download:

  1. Have valid authentication data for an account that owns the album(s) you
     wish to download
  2. API: album metadata (for track IDs)
  3. For each track:
     * API: request a permission token
     * download the track's M3U playlist file
     * download each fragment
     * decrypt each fragment (using token in stream file) & combine
     * transcode to a more palatable audio format (but keep the originals
       around!)

#### Combining track fragments
Using FFmpeg on Linux.

To combine all parts of a track (without transcoding), run the following in the
track's folder:

    ffmpeg -allowed_extensions ALL -i v1_h.m3u8 -c:a copy combined.ts

FFmpeg reads the playlist file and decrypts the parts for us on the fly.

#### Transcoding tracks
Using FFmpeg on Linux. These commands should be run in a folder with all the
tracks you wish to transcode.

When checked on 2022-12-07, Mimicle delivers track fragments using stereo AAC
(in the MPEG-TS container). It's pretty high quality, ~300kbps (or ~210kbps?? I
don't know how to read FFmpeg output).

Transcode as you would any audio file. raehik's suggestion and personal
preference is Opus 192k:

     for f in *.ts; do; ffmpeg -i "$f" -b:a 192k "${f%.*}".opus; done

Or use Ogg Vorbis, perhaps `-q:a 6` or `-b:a 192k`.

For good quality MP3, use `-qscale:a 0`:

     for f in *.ts; do; ffmpeg -i "$f" -codec:a libmp3lame -qscale:a 0 "${f%.*}".mp3; done

#### General notes
##### No DRM or watermarks
**There are no DRM or markers in audio streams.** Data is identical between
users and streaming sessions. They are providing a static, pre-chunked download.
(Stream keys do not change either.) Given a raw audio stream, there is no
tracing which account downloaded it.

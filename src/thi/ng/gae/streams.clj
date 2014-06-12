(ns thi.ng.gae.streams
  (:import
   [java.io File FileInputStream FileWriter InputStream OutputStream]
   [java.nio ByteBuffer]
   [java.nio.channels Channel Channels ReadableByteChannel WritableByteChannel]))

(defn copy-stream
  [^InputStream input, ^OutputStream output]
  (with-open [^ReadableByteChannel in-channel (Channels/newChannel input)
              ^WritableByteChannel out-channel (Channels/newChannel output)]
    (let [^ByteBuffer buf (ByteBuffer/allocateDirect (* 4 1024))]
      (loop []
        (when (>= (.read in-channel buf) 0)
          (.flip buf)
          (.write out-channel buf)
          (.compact buf)
          (recur)))
      (.flip buf)
      (loop [] ; drain the buffer
        (when (.hasRemaining buf)
          (.write out-channel buf)
          (recur))))))

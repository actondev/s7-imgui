#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
 
#include <curl/curl.h>
 
static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
}
 
int main(int argc, char *argv[])
{
  CURL *curl_handle;
  static const char *pagefilename = "page.out";
  FILE *pagefile;

  
  const char* url = "https://httpbin.org/get";
 
  curl_global_init(CURL_GLOBAL_ALL);
 
  /* init the curl session */ 
  curl_handle = curl_easy_init();
  
  // * SSL certificate problem: self signed certificate in certificate chain
//   curl_easy_setopt(curl_handle, CURLOPT_SSL_VERIFYHOST, 0L);
  curl_easy_setopt(curl_handle, CURLOPT_SSL_VERIFYPEER, 0L);
 
  /* set URL to get here */ 
  curl_easy_setopt(curl_handle, CURLOPT_URL, url);
 
  /* Switch on full protocol/debug output while testing */ 
  curl_easy_setopt(curl_handle, CURLOPT_VERBOSE, 1L);
 
  /* disable progress meter, set to 0L to enable it */ 
  curl_easy_setopt(curl_handle, CURLOPT_NOPROGRESS, 1L);
 
  /* send all data to this function  */ 
  curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_data);
 
  /* open the file */ 
  pagefile = fopen(pagefilename, "wb");
  if(pagefile) {
 
    /* write the page body to this file handle */ 
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, pagefile);
 
    /* get it! */ 
    CURLcode res = curl_easy_perform(curl_handle);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
 
    /* close the header file */ 
    fclose(pagefile);
  } else {
      printf("file not found!?\n");
  }
 
  /* cleanup curl stuff */ 
  curl_easy_cleanup(curl_handle);
 
  curl_global_cleanup();
 
  return 0;
}

angular.module('seashell-local-files', [])
  /**
   * Local file storage service, using localforage.js
   * Interface is pretty much the same as websocket_client.js
   */
  .service('localfiles', ['$q', 
      function($q) {
        "use strict";
        var self = this;

        /*
         * Save a file to local storage.
         * @param {string} name: project name
         * @param {string} file_name: filename
         * @param {string} file_content: The contents of the file
         */ 
        self.writeFile = function(name, file_name, file_content) {
          console.log("Writing: ", file_content);
          return $q.when(localforage.setItem(name + file_name, file_content));
        };


        self.readFile = function(name, file_name) {
          return $q.when(localforage.getItem(name + file_name)).then(
            function (contents) {
              console.log("localforage readFile");
              console.log(contents); 
              return contents; 
            });
        };


        self.deleteFile = function(name, file_name) {
          return $q.when(localforage.removeItem(name + file_name));
        };


  }]);

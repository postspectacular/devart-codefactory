/*global module:false*/
module.exports = function(grunt) {

  // Project configuration.
  grunt.initConfig({
    // Metadata.
    pkg: grunt.file.readJSON('package.json'),
    less: {
      dev: {
        options: {
          paths: "src-less",
          compress: false
        },
        files: {
          "war/css/app.css": "src-less/main.less"
        }
      },
      prod: {
        options: {
          paths: "src-less",
          compress: true,
        },
        files: {
          "war/css/app.css": "src-less/main.less"
        }
      }
    },
    htmlmin: {
      prod: {
        options: {
          removeComments: true,
          collapseWhitespace: true,
          keepClosingSlash: true,
          caseSensitive: true,
          minifyJS: true
        },
        files: {
          'war/index.html': 'src-html/index.html',
          'war/barbican.html': 'src-html/barbican.html',
          'war/workshop.html': 'src-html/workshop.html'
        }
      }
    },
    watch: {
      dev: {
        files: ['src-less/*.less','src-html/*.html'],
        tasks: ['less:dev','htmlmin:prod']
      },
      prod: {
        files: ['src-less/*.less','src-html/*.html'],
        tasks: ['less:prod','htmlmin:prod']
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-htmlmin');

  // Default task.
  grunt.registerTask('default', ['less:prod','htmlmin:prod','watch:prod']);
};

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
          compress: true
        },
        files: {
          "war/staging/css/app.css": "src-less/main.less"
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
      dev: {
        options: {
          removeComments: true,
          collapseWhitespace: true,
          keepClosingSlash: true,
          caseSensitive: true,
          minifyJS: true
        },
        files: {
          'war/staging/index.html': 'src-html/staging.html'
        }
      },
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
          'war/barbican/index.html': 'src-html/barbican.html',
          'war/workshop/index.html': 'src-html/workshop.html',
          'war/staging/index.html': 'src-html/staging.html'
        }
      }
    },
    copy: {
      applystage: {
        files: [{expand: true, cwd: 'war/staging/css/', src: ['*.css'], dest: 'war/css/'},
                {expand: true, cwd: 'war/staging/img/', src: ['**'], dest: 'war/img/'},
                {expand: true, cwd: 'war/staging/js/', src: ['app.js'], dest: 'war/js/'}]
      }
    },
    watch: {
      dev: {
        files: ['src-less/*.less','src-html/*.html'],
        tasks: ['less:dev','htmlmin:dev']
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
  grunt.loadNpmTasks('grunt-contrib-copy');

  // Default task.
  grunt.registerTask('default', ['less:dev','htmlmin:dev','watch:dev']);
};

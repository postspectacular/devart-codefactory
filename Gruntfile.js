/*global module:false*/
module.exports = function(grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    less: {
      dev: {
        options: {
          paths: "src-less",
          compress: true,
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
          "war/css/app.css": "src-less/main.less",
          "war/staging/css/app.css": "src-less/main.less"
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
          'war/staging/index.html': 'src-html/staging.html',
          'war/maintenance.html': 'src-html/maintenance.html'
        }
      }
    },
    watch: {
      dev: {
        files: ['src-less/*.less','src-html/*.html'],
        tasks: ['less','htmlmin:dev']
      },
      prod: {
        files: ['src-less/*.less','src-html/*.html'],
        tasks: ['less','htmlmin:prod']
      }
    },
    replace: {
      prod: {
        options: {
          patterns: [
            {
              match: 'timestamp',
              replacement: '<%= new Date().getTime() %>'
            }
          ]
        },
        files: [
          {src: ['war/index.html'], dest: 'war/index.html'},
          {src: ['war/barbican/index.html'], dest: 'war/barbican/index.html'},
          {src: ['war/workshop/index.html'], dest: 'war/workshop/index.html'},
          {src: ['war/staging/index.html'], dest: 'war/staging/index.html'},
          {src: ['war/maintenance.html'], dest: 'war/maintenance.html'}
        ]
      }
    }
  });

  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-htmlmin');
  grunt.loadNpmTasks('grunt-replace');

  // Default task.
  grunt.registerTask('default', ['less:dev','htmlmin:dev','watch:dev']);
  grunt.registerTask('prod', ['less:prod','htmlmin:prod','replace:prod']);
};

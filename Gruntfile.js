/*global module:false*/
module.exports = function(grunt) {

  // Project configuration.
  grunt.initConfig({
    // Metadata.
    pkg: grunt.file.readJSON('package.json'),
    banner: '/*! <%= pkg.title || pkg.name %> - v<%= pkg.version %> - ' +
      '<%= grunt.template.today("yyyy-mm-dd") %>\n' +
      '<%= pkg.homepage ? "* " + pkg.homepage + "\\n" : "" %>' +
      '* Copyright (c) <%= grunt.template.today("yyyy") %> <%= pkg.author.name %>;' +
      ' Licensed <%= _.pluck(pkg.licenses, "type").join(", ") %> */\n',
    // Task configuration.
    concat: {
      options: {
        banner: '<%= banner %>',
        stripBanners: true
      },
      dist: {
        src: ['lib/<%= pkg.name %>.js'],
        dest: 'dist/<%= pkg.name %>.js'
      }
    },
    less: {
      dev: {
        options: {
          paths: "src-less",
          compress: false
        },
        files: {
          "war/css-compiled/flextest.css": "src-less/flextest.less",
          "war/css-compiled/main-flex.css": "src-less/main-flex.less"
        }
      },
      prod: {
        options: {
          paths: "src-less",
          compress: true,
        },
        files: {
          "war/css-compiled/flextest.css": "src-less/flextest.less",
          "war/css-compiled/main-flex.css": "src-less/main-flex.less"
        }
      }
    },
    watch: {
      dev: {
        files: 'src-less/*.less',
        tasks: ['less:dev']
      },
      prod: {
        files: '<%= less.prod.options.paths %>',
        tasks: ['less:prod']
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-watch');

  // Default task.
  grunt.registerTask('default', ['less','watch']);
};

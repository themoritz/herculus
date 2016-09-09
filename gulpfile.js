var browserSync = require('browser-sync')
var gulp = require('gulp')
var shell = require('gulp-shell')

var exec = require('child_process').exec;

gulp.task('combine', function (cb) {
  exec('bash ./combine-js-app.sh', function (err, stdout, stderr) {
    console.log(stdout);
    console.log(stderr);
    cb(err);
  });
})

gulp.task('watch', function () {
  browserSync.init(null, {
    proxy: 'http://localhost:3000',
    port: 3001,
    open: false
  })
  // gulp.watch('assets/public/js/bundle.js', ['combine'])
  gulp.watch('assets/public/js/app.js', browserSync.reload)
  gulp.watch('assets/public/css/*.css', function () {
    gulp.src('assets/public/css/*.css')
      .pipe(browserSync.stream())
  })
})

gulp.task('default', ['watch'])

var browserSync = require('browser-sync')
var gulp = require('gulp')

gulp.task('default', function () {
  browserSync.init(null, {
    proxy: 'http://localhost:3000',
    port: 3001,
    open: false
  })
  gulp.watch('public/js/app.js', browserSync.reload)
  gulp.watch('public/css/*.css', function () {
    gulp.src('public/css/*.css')
      .pipe(browserSync.stream())
  })
})

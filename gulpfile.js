var browserSync = require('browser-sync')
var gulp = require('gulp')

gulp.task('watch', function () {
    browserSync.init(null, {
        proxy: 'http://localhost:3000',
        port: 3001,
        open: false
    })
    // gulp.watch('assets/public/js/*', browserSync.reload)
    gulp.watch('assets/public/css/*', function () {
      gulp.src('assets/public/css/*')
        .pipe(browserSync.stream())
    })
})

gulp.task('default', ['watch'])

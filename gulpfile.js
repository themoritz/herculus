var browserSync = require('browser-sync')
var sass = require('gulp-sass')
var gulp = require('gulp')

gulp.task('watch', function () {
    browserSync.init(null, {
        proxy: 'http://localhost:3000',
        port: 3001,
        open: false
    })
    gulp.watch('assets/public/js/main.js', browserSync.reload)
    gulp.watch('assets/sass/**/*.scss', function () {
        gulp.src('assets/sass/**/*.scss')
            .pipe(sass({
                errLogToConsole: true
            })).on('error', function (err) {
                console.log(err)
                this.emit('end')
            })
            .pipe(gulp.dest('assets/public/css'))
            .pipe(browserSync.stream())
    })
})

gulp.task('default', ['watch'])

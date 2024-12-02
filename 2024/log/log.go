package log

import (
	"go.uber.org/zap"
)

var sugar *zap.SugaredLogger

func init() {
	const stackSkip = 1
	logger, _ := zap.NewDevelopment(zap.AddCallerSkip(stackSkip))
	sugar = logger.Sugar()
}

func Info(args ...interface{}) {
	sugar.Info(args...)
}

func Infof(template string, args ...interface{}) {
	sugar.Infof(template, args...)
}

func Fatal(args ...interface{}) {
	sugar.Fatal(args...)
}

func Fatalf(template string, args ...interface{}) {
	sugar.Fatalf(template, args...)
}

func Error(args ...interface{}) {
	sugar.Error(args...)
}

func Errorf(template string, args ...interface{}) {
	sugar.Errorf(template, args...)
}

// InfoIfErr if err is not nil, call Info on it.
func InfoIfErr(err error) {
	if err != nil {
		sugar.Info(err)
	}
}

// FatalIfErr if err is not nil, call Fatal on it.
func FatalIfErr(err error) {
	if err != nil {
		sugar.Fatal(err)
	}
}

// Sync flushes buffed logs.
func Sync() {
	sugar.Sync()
}

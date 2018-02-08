default = [
  '-x',
  'c++'
]

include_paths = [
  '-I',
  'C:\\Qt\\5.9.1\\msvc2015_64\\include\\',
  '-I',
  'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtWidgets',
  '-I',
  'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtGui',
  '-I',
  'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtCharts',
  '-I',
  'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtCore'
]


flags = default + include_paths;

def FlagsForFile( filename ):
  return { 'flags': flags }

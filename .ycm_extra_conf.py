import os

default = ['-Wall', '-Wextra', '-Werror', '-std=c++11', '-x', 'c++']

if os.name == 'posix':
    default += [
        '-isystem', '/usr/include/c++/7', '-isystem',
        '/usr/include/c++/7/backward', '-isystem', '/usr/local/include',
        '-isystem', '/usr/include'
    ]

if os.name == 'nt':  # If on Windows
    include_paths = [
        '-I', 'C:\\Qt\\5.9.1\\msvc2015_64\\include\\', '-I',
        'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtWidgets', '-I',
        'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtGui', '-I',
        'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtCharts', '-I',
        'C:\\Qt\\5.9.1\\msvc2015_64\\include\\QtCore'
    ]
else:
    include_paths = []

flags = default + include_paths


def FlagsForFile(filename):
    return {'flags': flags}

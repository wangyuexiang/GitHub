QT += widgets

SOURCES   = adddialog.cpp \
            terminalwidget.cpp \
            main.cpp \
            mainwindow.cpp \
            terminalinfotab.cpp \
            productmodel.cpp \
            tablemodel.cpp
HEADERS   = adddialog.h \
            terminalwidget.h \
            mainwindow.h \
            terminalinfotab.h \
            productmodel.h\
            product.h \
              tablemodel.h

# install
target.path = /usr/local/lib
INSTALLS += target

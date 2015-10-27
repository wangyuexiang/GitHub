#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "terminalwidget.h"

#include <QMainWindow>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();

private slots:
    void updateActions(const QItemSelection &selection);
    void openFile();
    void saveFile();

private:
    void createMenus();

    TerminalWidget *terminalWidget;
    QMenu *fileMenu;
    QMenu *toolMenu;
    QAction *openAct;
    QAction *saveAct;
    QAction *exitAct;
    QAction *addAct;
    QAction *editAct;
    QAction *removeAct;
		
    QMenu *productMenu;
    QAction *addProduct;
    QAction *editProduct;
    QAction *removeProduct;
};

#endif // MAINWINDOW_H

#ifndef TERMINALWINDOW_H
#define TERMINALWINDOW_H

#include "productmodel.h"

#include <QMainWindow>

#include <QAbstractTableModel>
#include <QSortFilterProxyModel>


class TerminalWindow : public QMainWindow
{
    Q_OBJECT

public:
    TerminalWindow();

private slots:
    void updateActions(const QItemSelection &selection);
    void openFile();
    void saveFile();

private:
    void createMenus();

		// Widget *widget;
		QMenu *fileMenu;
    QMenu *toolMenu;
    QAction *openAct;
    QAction *saveAct;
    QAction *exitAct;
    QAction *addAct;
    QAction *editAct;
    QAction *removeAct;
};
//! [0]

#endif // TERMINALWINDOW_H

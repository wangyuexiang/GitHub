#include "terminalwindow.h"

#include <QAction>
#include <QFileDialog>
#include <QMenuBar>

//! [0]
TerminalWindow::TerminalWindow()
{
    // widget = new Widget;
    // setCentralWidget(widget);
    createMenus();
    setWindowTitle(tr("Warehouse Terminal"));
}
//! [0]

//! [1a]
void TerminalWindow::createMenus()
{
    fileMenu = menuBar()->addMenu(tr("&Terminal"));

    openAct = new QAction(tr("&Open..."), this);
    fileMenu->addAction(openAct);
    connect(openAct, SIGNAL(triggered()), this, SLOT(openFile()));
//! [1a]

    saveAct = new QAction(tr("&Save As..."), this);
    fileMenu->addAction(saveAct);
    connect(saveAct, SIGNAL(triggered()), this, SLOT(saveFile()));

    fileMenu->addSeparator();

    exitAct = new QAction(tr("E&xit"), this);
    fileMenu->addAction(exitAct);
    connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

    toolMenu = menuBar()->addMenu(tr("&Orders"));

    addAct = new QAction(tr("&Add Order..."), this);
    toolMenu->addAction(addAct);
    // connect(addAct, SIGNAL(triggered()), addressWidget, SLOT(addEntry()));

//! [1b]
    editAct = new QAction(tr("&Edit Order..."), this);
    editAct->setEnabled(false);
    toolMenu->addAction(editAct);
    // connect(editAct, SIGNAL(triggered()), addressWidget, SLOT(editEntry()));

    toolMenu->addSeparator();

    removeAct = new QAction(tr("&Cancel Order"), this);
    removeAct->setEnabled(false);
    toolMenu->addAction(removeAct);
    // connect(removeAct, SIGNAL(triggered()), addressWidget, SLOT(removeEntry()));

    // connect(addressWidget, SIGNAL(selectionChanged(QItemSelection)),
        // this, SLOT(updateActions(QItemSelection)));

}
//! [1b]

//! [2]
void TerminalWindow::openFile()
{
    QString fileName = QFileDialog::getOpenFileName(this);
    // if (!fileName.isEmpty())
        // addressWidget->readFromFile(fileName);
}
//! [2]

//! [3]
void TerminalWindow::saveFile()
{
    QString fileName = QFileDialog::getSaveFileName(this);
    // if (!fileName.isEmpty())
        // addressWidget->writeToFile(fileName);
}
//! [3]

//! [4]
void TerminalWindow::updateActions(const QItemSelection &selection)
{
    // QModelIndexList indexes = selection.indexes();

    // if (!indexes.isEmpty()) {
        // removeAct->setEnabled(true);
        // editAct->setEnabled(true);
    // } else {
        // removeAct->setEnabled(false);
        // editAct->setEnabled(false);
    // }
}
//! [4]

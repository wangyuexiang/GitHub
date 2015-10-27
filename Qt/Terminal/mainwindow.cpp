#include "mainwindow.h"

#include <QAction>
#include <QFileDialog>
#include <QMenuBar>

MainWindow::MainWindow()
{
    terminalWidget = new TerminalWidget;
    setCentralWidget(terminalWidget);
    createMenus();
    setWindowTitle(tr("Warehouse Terminal"));
}

void MainWindow::createMenus()
{
		// fileMenu
    fileMenu = menuBar()->addMenu(tr("&Terminal"));

    openAct = new QAction(tr("&Open..."), this);
    fileMenu->addAction(openAct);
    connect(openAct, SIGNAL(triggered()), this, SLOT(openFile()));

    saveAct = new QAction(tr("&Save As..."), this);
    fileMenu->addAction(saveAct);
    connect(saveAct, SIGNAL(triggered()), this, SLOT(saveFile()));

    fileMenu->addSeparator();

    exitAct = new QAction(tr("E&xit"), this);
    fileMenu->addAction(exitAct);
    connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));
	
		// toolMenu
    toolMenu = menuBar()->addMenu(tr("&Orders"));

    addAct = new QAction(tr("&Add Order..."), this);
    toolMenu->addAction(addAct);
    connect(addAct, SIGNAL(triggered()), terminalWidget, SLOT(addEntry()));

    editAct = new QAction(tr("&Edit Order..."), this);
    // editAct->setEnabled(false);
    toolMenu->addAction(editAct);
    connect(editAct, SIGNAL(triggered()), terminalWidget, SLOT(editEntry()));

    toolMenu->addSeparator();

    removeAct = new QAction(tr("&Cancel Order"), this);
    // removeAct->setEnabled(false);
    toolMenu->addAction(removeAct);
    connect(removeAct, SIGNAL(triggered()), terminalWidget, SLOT(removeEntry()));
		
		// productMenu
		productMenu = menuBar()->addMenu(tr("&Products"));

    addAct = new QAction(tr("&Add Product..."), this);
    productMenu->addAction(addAct);
    connect(addAct, SIGNAL(triggered()), terminalWidget, SLOT(addProduct()));

    editAct = new QAction(tr("&Edit Product..."), this);
    editAct->setEnabled(false);
    productMenu->addAction(editAct);
    connect(editAct, SIGNAL(triggered()), terminalWidget, SLOT(editProduct()));

    productMenu->addSeparator();

    removeAct = new QAction(tr("&Remove Product"), this);
    removeAct->setEnabled(false);
    productMenu->addAction(removeAct);
    connect(removeAct, SIGNAL(triggered()), terminalWidget, SLOT(removeProduct()));

    connect(terminalWidget, SIGNAL(selectionChanged(QItemSelection)),
        this, SLOT(updateActions(QItemSelection)));

}

void MainWindow::openFile()
{
    QString fileName = QFileDialog::getOpenFileName(this);
    if (!fileName.isEmpty())
        terminalWidget->readFromFile(fileName);
}

void MainWindow::saveFile()
{
    QString fileName = QFileDialog::getSaveFileName(this);
    if (!fileName.isEmpty())
        terminalWidget->writeToFile(fileName);
}

void MainWindow::updateActions(const QItemSelection &selection)
{
    QModelIndexList indexes = selection.indexes();

    if (!indexes.isEmpty()) {
        removeAct->setEnabled(true);
        editAct->setEnabled(true);
    } else {
        removeAct->setEnabled(false);
        editAct->setEnabled(false);
    }
}

#ifndef TERMINALWIDGET_H
#define TERMINALWIDGET_H

#include "terminalinfotab.h"
#include "tablemodel.h"
#include "productmodel.h"

#include <QItemSelection>
#include <QTabWidget>

QT_BEGIN_NAMESPACE
class QSortFilterProxyModel;
class QItemSelectionModel;
QT_END_NAMESPACE

class TerminalWidget : public QTabWidget
{
    Q_OBJECT

public:
    TerminalWidget(QWidget *parent = 0);
    void readFromFile(const QString &fileName);
    void writeToFile(const QString &fileName);

public slots:
    void addEntry();
    void addEntry(QString name, QString address);
    void editEntry();
    void removeEntry();
		
		void addProduct();
    void addProduct(int i, int c, double w, int number1, int number2, int number3);
    void editProduct();
    void removeProduct();

signals:
    void selectionChanged (const QItemSelection &selected);

private:
    void setupTabs();

    TableModel *table;
    ProductModel *producttable;
		
    TerminalInfoTab *terminalInfoTab;
    QSortFilterProxyModel *table_proxy;
    QSortFilterProxyModel *product_proxy;
};

#endif // TERMINALWIDGET_H

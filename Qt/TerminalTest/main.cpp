// #include "terminalwindow.h"

#include "productmodel.h"

#include <QApplication>
#include <QAbstractTableModel>
#include <QSortFilterProxyModel>
#include <QInputDialog>
#include <QTableView>
#include <QGridLayout>
#include <QPushButton>
#include <QDebug>

class Widget : public QWidget {
    Q_OBJECT
    ProductModel m_model;
    QSortFilterProxyModel m_proxy;
    // Q_SLOT void setFilter() {
        // QInputDialog * dialog = new QInputDialog(this);
        // dialog->setLabelText("Enter registration number fragment to filter on. Leave empty to clear filter.");
        // dialog->setInputMode(QInputDialog::TextInput);
        // dialog->open(&m_proxy, SLOT(setFilterFixedString(QString)));
        // dialog->setAttribute(Qt::WA_DeleteOnClose);
    // }
public:
    Widget() {
        QGridLayout * layout = new QGridLayout(this);
        QTableView * view = new QTableView;
        layout->addWidget(view, 0, 0, 1, 1);
        // QPushButton * btn = new QPushButton("Filter");
        // layout->addWidget(btn, 1, 0, 1, 1);
        // connect(btn, SIGNAL(clicked()), SLOT(setFilter()));
        m_model.append(Product(1,2,2.5,10,20,20));
        m_model.append(Product(2,4,25,16,20,20));
        m_model.append(Product(1,2,2.5,10,25,20));
        m_model.append(Product(3,2,5,0,20,20));

        m_model.listOfProducts.insert(2,Product(4025,2,5,0,20,20));
        QList<Product> list = m_model.getList();
  /*      Product p = list.value(1);
        p.setCategory(10250);
        m_model.append(p);

        p.setCategory(20250);
        list.replace(0,p);
        */
        ProductModel p_model(list);
        p_model.append(Product(2,4,25,16,20,20));

        m_model.listOfProducts = p_model.listOfProducts;

        m_proxy.setSourceModel(&p_model);
        m_proxy.setFilterKeyColumn(2);
        view->setModel(&m_proxy);
    }
};

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    Widget w;
    w.show();
    return a.exec();
}

#include "main.moc"

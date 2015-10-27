#ifndef PRODUCTMODEL_H
#define PRODUCTMODEL_H

#include <QAbstractTableModel>
#include <QList>

#include "product.h"

class ProductModel : public QAbstractTableModel {
		Q_OBJECT
public:
    ProductModel(QObject *parent = 0);
    ProductModel(QList<Product > listOfProducts, QObject *parent = 0);

    int rowCount(const QModelIndex &parent) const Q_DECL_OVERRIDE;
    int columnCount(const QModelIndex &parent) const Q_DECL_OVERRIDE;
    QVariant data(const QModelIndex &index, int role) const Q_DECL_OVERRIDE;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const Q_DECL_OVERRIDE;
		
		void append(const Product & product) Q_DECL_OVERRIDE;

    // Qt::ItemFlags flags(const QModelIndex &index) const Q_DECL_OVERRIDE;
    // bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) Q_DECL_OVERRIDE;
    // bool insertRows(int position, int rows, const QModelIndex &index = QModelIndex()) Q_DECL_OVERRIDE;
    // bool removeRows(int position, int rows, const QModelIndex &index = QModelIndex()) Q_DECL_OVERRIDE;
    QList<Product> getList();
		
//private:
    QList<Product> listOfProducts;
};


#endif // PRODUCTMODEL_H


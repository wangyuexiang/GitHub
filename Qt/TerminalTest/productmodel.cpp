#include "productmodel.h"

ProductModel::ProductModel(QObject *parent)
    : QAbstractTableModel(parent)
{
}

ProductModel::ProductModel(QList<Product> products, QObject *parent)
    : QAbstractTableModel(parent)
{
    listOfProducts = products;
}

int ProductModel::rowCount(const QModelIndex &parent) const 
{ 
		Q_UNUSED(parent);
		return listOfProducts.count(); 
}

int ProductModel::columnCount(const QModelIndex &parent) const 
{ 
		Q_UNUSED(parent);
		return 6; 
}

QVariant ProductModel::data(const QModelIndex &index, int role) const 
{
		if (role != Qt::DisplayRole && role != Qt::EditRole) 
			return QVariant();
		
		Product product = listOfProducts[index.row()];
		switch (index.column()) {
			case 0: return product.getItem();
			case 1: return product.getCategory();
			case 2: return product.getWeight();
			case 3: return product.getNumber1();
			case 4: return product.getNumber2();
			case 5: return product.getNumber3();
			default: return QVariant();
		};
}

QVariant ProductModel::headerData(int section, Qt::Orientation orientation, int role) const 
{
		if (orientation != Qt::Horizontal) 
			return QVariant();
		if (role != Qt::DisplayRole) 
			return QVariant();
		switch (section) {
			case 0: return "Item";
			case 1: return "Category";
			case 2: return "Weight";
			case 3: return "Number1";
			case 4: return "Number2";
			case 5: return "Number3";
			default: return QVariant();
		}
}

void ProductModel::append(const Product & product) {
		beginInsertRows(QModelIndex(), listOfProducts.count(), listOfProducts.count());
		listOfProducts.append(product);
		endInsertRows();
}

QList<Product> ProductModel::getList()
{
    return listOfProducts;
}

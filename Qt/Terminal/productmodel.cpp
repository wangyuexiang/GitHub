#include "productmodel.h"

#include <string>

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

// void ProductModel::append(const Product & product) {
		// beginInsertRows(QModelIndex(), listOfProducts.count(), listOfProducts.count());
		// listOfProducts.append(product);
		// endInsertRows();
// }

bool ProductModel::insertRows(int position, int rows, const QModelIndex &index)
{
    Q_UNUSED(index);
    beginInsertRows(QModelIndex(), position, position + rows - 1);

    for (int row = 0; row < rows; ++row) {
        Product product(0,0,1,0,0,0);
        listOfProducts.insert(position, product);
    }

    endInsertRows();
    return true;
}

void ProductModel::append(const Product & product) {
		beginInsertRows(QModelIndex(), listOfProducts.count(), listOfProducts.count());
		listOfProducts.append(product);
		endInsertRows();
}

bool ProductModel::removeRows(int position, int rows, const QModelIndex &index)
{
    Q_UNUSED(index);
    beginRemoveRows(QModelIndex(), position, position + rows - 1);

    for (int row = 0; row < rows; ++row) {
        listOfProducts.removeAt(position);
    }

    endRemoveRows();
    return true;
}

bool ProductModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (index.isValid() && role == Qt::EditRole) {
        int row = index.row();

				Product p = listOfProducts.value(row);
				
				switch (index.column()) {
					case 0: p.setItem(value.toInt());
					case 1: p.setCategory(value.toInt());
					case 2: p.setWeight(value.toDouble());
					case 3: p.setNumber1(value.toInt());
					case 4: p.setNumber2(value.toInt());
					case 5: p.setNumber3(value.toInt());
				default: return false;
				}
				
        listOfProducts.replace(row, p);
        emit(dataChanged(index, index));

        return true;
    }
    return false;
}

bool ProductModel::insertProduct(int row, const Product & product)
{
	 listOfProducts.replace(row, product);
	 return true;
}

Qt::ItemFlags ProductModel::flags(const QModelIndex &index) const
{
    if (!index.isValid())
        return Qt::ItemIsEnabled;

    return QAbstractTableModel::flags(index) | Qt::ItemIsEditable;
}

QList<Product> ProductModel::getList()
{
    return listOfProducts;
}
#include "adddialog.h"

#include <QtWidgets>

AddDialog::AddDialog(QWidget *parent)
    : QDialog(parent)
{
    nameLabel = new QLabel("Priority");
    addressLabel = new QLabel("Item");
    okButton = new QPushButton("OK");
    cancelButton = new QPushButton("Cancel");

    nameText = new QLineEdit;
    addressText = new QTextEdit;

    QGridLayout *gLayout = new QGridLayout;
    gLayout->setColumnStretch(1, 2);
    gLayout->addWidget(nameLabel, 0, 0);
    gLayout->addWidget(nameText, 0, 1);

    gLayout->addWidget(addressLabel, 1, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(addressText, 1, 1, Qt::AlignLeft);

    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addWidget(okButton);
    buttonLayout->addWidget(cancelButton);

    gLayout->addLayout(buttonLayout, 2, 1, Qt::AlignRight);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addLayout(gLayout);
    setLayout(mainLayout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
    connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));

    setWindowTitle(tr("Add an Order"));
}

AddProductDialog::AddProductDialog(QWidget *parent)
    : QDialog(parent)
{
    itemLabel = new QLabel("Item");
		categoryLabel = new QLabel("Category");
		weightLabel = new QLabel("Weight");
		number1Label = new QLabel("Number1");
		number2Label = new QLabel("Number2");
		number3Label = new QLabel("Number3");
    okButton = new QPushButton("OK");
    cancelButton = new QPushButton("Cancel");

		itemText = new QLineEdit;
		categoryText = new QLineEdit;
		weightText = new QLineEdit;
		number1Text = new QLineEdit;
		number2Text = new QLineEdit;
		number3Text = new QLineEdit;

    QGridLayout *gLayout = new QGridLayout;
    gLayout->setColumnStretch(1, 2);
    gLayout->addWidget(itemLabel, 0, 0);
    gLayout->addWidget(itemText, 0, 1, Qt::AlignLeft);

    gLayout->addWidget(categoryLabel, 1, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(categoryText, 1, 1, Qt::AlignLeft);
		
    gLayout->addWidget(weightLabel, 2, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(weightText, 2, 1, Qt::AlignLeft);
		
    gLayout->addWidget(number1Label, 3, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(number1Text, 3, 1, Qt::AlignLeft);
		
    gLayout->addWidget(number2Label, 4, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(number2Text, 4, 1, Qt::AlignLeft);
		
    gLayout->addWidget(number3Label, 5, 0, Qt::AlignLeft|Qt::AlignTop);
    gLayout->addWidget(number3Text, 5, 1, Qt::AlignLeft);

    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addWidget(okButton);
    buttonLayout->addWidget(cancelButton);

    gLayout->addLayout(buttonLayout, 6, 1, Qt::AlignRight);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addLayout(gLayout);
    setLayout(mainLayout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
    connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));

    setWindowTitle(tr("Add a Product"));
}

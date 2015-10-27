#include "adddialog.h"
#include "terminalinfotab.h"

#include <QtWidgets>

TerminalInfoTab::TerminalInfoTab(QWidget *parent)
{
    Q_UNUSED(parent);

    descriptionLabel = new QLabel(tr("This tab is used to display Termnial & Operator Information. "
                                      "\nClick to add new Order/Product."));

    addOrderButton = new QPushButton(tr("Add an Order"));
    addProductButton = new QPushButton(tr("Add a Product"));

    connect(addOrderButton, SIGNAL(clicked()), this, SLOT(addEntry()));
    connect(addProductButton, SIGNAL(clicked()), this, SLOT(addProduct()));

    mainLayout = new QVBoxLayout;
    mainLayout->addWidget(descriptionLabel);
		
		// QHBoxLayout *buttonLayout = new QHBoxLayout;
    // buttonLayout->addWidget(addOrderButton);
    // buttonLayout->addWidget(addProductButton);
		
		QGridLayout *buttonLayout = new QGridLayout;
		buttonLayout->setColumnStretch(1, 2);
    buttonLayout->addWidget(addOrderButton, 0, 0);
    buttonLayout->addWidget(addProductButton, 0, 1, Qt::AlignLeft);
		
    mainLayout->addLayout(buttonLayout, Qt::AlignCenter);

    setLayout(mainLayout);
}

void TerminalInfoTab::addEntry()
{
    AddDialog aDialog;

    if (aDialog.exec()) {
        QString name = aDialog.nameText->text();
        QString address = aDialog.addressText->toPlainText();

        emit sendDetails(name, address);
    }
}

void TerminalInfoTab::addProduct()
{
    AddProductDialog aDialog;

    if (aDialog.exec()) {
        int item = aDialog.itemText->text().toInt();
        int category = aDialog.categoryText->text().toInt();
        double weight = aDialog.weightText->text().toDouble();
        int number1 = aDialog.number1Text->text().toInt();
        int number2 = aDialog.number2Text->text().toInt();
        int number3 = aDialog.number3Text->text().toInt();
				
        emit sendDetails(item,category,weight,number1,number2,number3);
    }
}

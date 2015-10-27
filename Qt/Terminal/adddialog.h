#ifndef ADDDIALOG_H
#define ADDDIALOG_H

#include <QDialog>

QT_BEGIN_NAMESPACE
class QLabel;
class QPushButton;
class QTextEdit;
class QLineEdit;
QT_END_NAMESPACE

class AddDialog : public QDialog
{
    Q_OBJECT

public:
    AddDialog(QWidget *parent = 0);
    QLineEdit *nameText;
    QTextEdit *addressText;

private:
    QLabel *nameLabel;
    QLabel *addressLabel;
    QPushButton *okButton;
    QPushButton *cancelButton;
};

class AddProductDialog : public QDialog
{
    Q_OBJECT

public:
    AddProductDialog(QWidget *parent = 0);
    QLineEdit *itemText;
		QLineEdit *categoryText;
		QLineEdit *weightText;
		QLineEdit *number1Text;
		QLineEdit *number2Text;
		QLineEdit *number3Text;

private:
    QLabel *itemLabel;
		QLabel *categoryLabel;
		QLabel *weightLabel;
		QLabel *number1Label;
		QLabel *number2Label;
		QLabel *number3Label;
    QPushButton *okButton;
    QPushButton *cancelButton;
};

#endif // ADDDIALOG_H

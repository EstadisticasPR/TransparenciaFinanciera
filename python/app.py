from flask import Flask, render_template, flash, request
from flask.ext.wtf import Form
from wtforms import SubmitField
from wtforms.validators import Required
from api_req import get_tf_data

# App Config
DEBUG = True
app = Flask(__name__)

class SubmitForm(Form):
    submit = SubmitField('Submit')

@app.route("/download_data", methods=['GET', 'POST'])
def get_data():
    form = SubmitForm(request.form)

    if request.method == 'POST':
        get_tf_data()
        flash("Los datos mas recientes han sido descargados para el Sistema de Transparencia Financiera de Puerto Rico")

    return render_template('download_data.html', form=form)

if __name__ == '__main__':
    app.run()

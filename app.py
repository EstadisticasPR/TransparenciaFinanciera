from flask import Flask, render_template, flash, request
from flask.ext.wtf import Form
from wtforms import SubmitField
from wtforms.validators import Required
from flask_simplelogin import SimpleLogin
from flask_simplelogin import login_required
from api_req import get_tf_data


app = Flask(__name__)
app.config['SECRET_KEY'] = '7d441f27d441f27567d441f2b6176a'

SimpleLogin(app)

class SubmitForm(Form):
    submit = SubmitField('Submit')

@app.route("/download_data", methods=['GET', 'POST'])
@login_required
def get_data():
    form = SubmitForm(request.form)

    if request.method == 'POST':
        get_tf_data()
        flash("Los datos mas recientes han sido descargados para el Sistema de Transparencia Financiera de Puerto Rico")

    return render_template('download_data.html', form=form)

if __name__ == '__main__':
    app.run()

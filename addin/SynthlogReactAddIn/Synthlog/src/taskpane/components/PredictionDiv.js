import * as React from 'react';
import { Button, ButtonType } from 'office-ui-fabric-react';
import { any } from 'prop-types';

export class PredictionDiv extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state={
            features: any,
            range_to_pred: any,
        };
    }

    render() {
        const {
            parent
        } = this.props;
        return (
            <div id='prediction'>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.setFeatures.bind(this)}>Set Features</Button>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.setRangeToPred.bind(this)}>To Predict</Button>
                    <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.predict.bind(this)}>Predict!</Button>
                    
            </div>
        );
    }

    setFeatures() {
        var that=this;
        /* Sets the current range to be the train set */
       Excel.run(context => {
             const range = context.workbook.getSelectedRange();
             range.format.fill.color = '#6da7de';
             that.setState({features:range});
             return context.sync();
         });
    }

    setRangeToPred() {
        var that=this;
        Excel.run(context => {
            const range = context.workbook.getSelectedRange();
            range.format.fill.color = '#943fa6';
            that.setState({range_to_pred:range});
            return context.sync();
        });
    }

    predict(){
        var that=this;
    }
}
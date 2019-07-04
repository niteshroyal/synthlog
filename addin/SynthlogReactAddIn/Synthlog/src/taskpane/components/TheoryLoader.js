import * as React from 'react';
import Select from 'react-select';
import { PrimaryButton, Button, ButtonType } from 'office-ui-fabric-react';

export default class TheoryLoader extends React.Component {
    render() {        
        const {
            active,
            theories
        } = this.props;

        return (
            <div id='theories'>
                <h3>Move to another theory</h3>
                <Select 
                    name="theory" 
                    options={ theories } 
                    value={ active }
                />
                <PrimaryButton
                    title="move"
                    color="red"
                    split={true}
                >
                    Move
                </PrimaryButton>
            </div>
        );
    }
}
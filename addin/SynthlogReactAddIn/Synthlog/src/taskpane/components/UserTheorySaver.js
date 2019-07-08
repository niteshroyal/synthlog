import * as React from 'react';
import { TextField, PrimaryButton } from 'office-ui-fabric-react';

export default class UserTheorySaver extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {
            message: '',
            sendable: false
        };
        this.parent = null;
        this.theory = '';
    }

    render() {
         const {
            parent
        } = this.props;
        this.parent = parent;

        try {
        return (
            <div id="saver">
                <h3>Save current state as theory</h3>
                    <TextField 
                        placeholder="Give a name to the new theory" 
                        onChange={ this.updateTheoryName.bind(this) }
                    />
                    <PrimaryButton 
                        text = "Save"
                        onClick={ this.submitHandler.bind(this) }
                    />
                    <p>{ this.state.message }</p>
                <hr/>
            </div>
        );
        }
        catch(err) {
            return (<p>{err.message}</p>)
        }
    }

    submitHandler() {
        if (this.parent != null && this.theory != ''){
            try {
            this.parent.saveTheory(this.theory);
            }
            catch(err) {
                this.setState({
                    message: err.message
                })
            }
        }
        else {
            this.setState({
                message :"parent: " + this.parent + ", theory name: " + this.theory
            });
        }
    }

    updateTheoryName(event, value) {
        this.theory = value;
    }
} 
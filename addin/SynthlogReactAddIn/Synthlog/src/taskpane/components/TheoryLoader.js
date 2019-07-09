import * as React from 'react';
import Select from 'react-select';
import { css, DefaultButton, PrimaryButton, Button, ButtonType, HighContrastSelector } from 'office-ui-fabric-react';

export default class TheoryLoader extends React.Component {
    constructor(props, context) {
        super(props, context);
        this.state = {
            active: '',
            click: ''
        }
        this.default_active = '';
        this.parent = null;
    }

    itemSelectionHandler = async(event, item) => {
        this.setState({active: item.key});
    }

    clickHandler = async() => {
        if (this.parent != null) {
            var that = this;
            var parameters = this.parent.generateSynthlogParameters({
                scope: this.state.active,
                script: 'builtin/load_active.pl'
            });
            this.parent.runSynthlog(parameters)
            .then(function(json) {
                that.setState({
                    click: 'json: ' + json.output
                })
            })
            .catch(err => that.setState({click: 'Nope'}));
        }
        else
            this.setState({click: 'clicked'});
    }

    render() {        
        const {
            active,
            parent,
            theories
        } = this.props;
        if (active && active != this.default_active) {
            this.state.active = active;
            this.default_active = active;
        }       
        this.parent = parent;

        try {
            var items = this.theoriesToItems(theories);
            return (
                <div id='theories'>
                    <h3>Move to another theory</h3>
                    <DefaultButton
                        primary
                        text={ this.state.active }
                        split={ true }
                        onClick={this.clickHandler.bind(this)}
                        menuProps={{
                                items: items,
                                onItemClick: this.itemSelectionHandler.bind(this)
                        }}
                    >
                        Move
                    </DefaultButton>
                    <p>{this.state.click}</p>
                    <hr/>
                </div>
            );
        }
        catch(err) {
            return (<p>{err.message}</p>)
        }
    }

    theoriesToItems(theories) {
        var items = [];
        theories.forEach(element => {
            items.push({key: element, text: element});
        });
        return items;
    }
}
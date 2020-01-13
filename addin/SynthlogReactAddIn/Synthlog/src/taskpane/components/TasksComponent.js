import * as React from 'react';
import {Button, ButtonType} from 'office-ui-fabric-react';

export default class TasksComponent extends React.Component {
    constructor(props, context) {
        super(props, context);
        // this.parent = this.props.parent;

        // this.state = {
        //     state_id: 0
        // };
    }

    render() {
        console.log(this.props.tasks);
        return (
            <div id="tasks">
                <h3>Tasks</h3>
                <div>
                    {this.props.tasks.map((t) => {
                        return (<p key={"task" + t.id}><Button
                            key={"task_button" + t.id}
                            className='normal-button'
                            buttonType={ButtonType.hero}
                            onClick={function() {this.props.callback(t.id)}.bind(this)}>
                                {t.name}
                        </Button></p>)
                    })}
                </div>
            </div>

        );

        // var items = this.state.tasks_suggestions;
        // try {
        //     return (
        //         <div id="tasks">
        //             <div>
        //                 <h3>Tasks</h3>
        //                 <Button className='normal-button' buttonType={ButtonType.hero}
        //                         onClick={this.createState.bind(this)}>Init</Button>
        //                 <p>State id: {this.state.state_id}</p>
        //
        //             </div>
        //             <hr/>
        //         </div>
        //     );
        // } catch (err) {
        //     fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`)
        // }
    }

    // createState() {
    //     this.parent.createState().then(json => this.setState({state_id: json.id}));
    // }
    //
    // _onRenderCell(item, index) {
    //     var that = this;
    //     return (
    //         <Button className='normal-button' buttonType={ButtonType.hero} onClick={(e) => {
    //             this.performTask(item.id).bind(this);
    //         }}>{item.descr}</Button>
    //         //<div><p style={{ color: colors[index] }}>{name}</p></div>
    //     )
    // }
    //
    // callMERCS = async () => {
    //     var that = this;
    //     var parameters = {file: Office.context.document.url};
    //     return fetch(`${this.parent.api}/call_mercs`, {
    //         method: 'POST',
    //         headers: {
    //             'Accept': 'application/json',
    //             'Content-Type': 'application/json'
    //         },
    //         body: JSON.stringify(parameters)
    //     })
    //         .then(response => response.json())
    //         .then(function (json) {
    //             fetch(`${that.parent.api}/log?type=mercs_answer&message=${json}`)
    //             return json;
    //         })
    //         .catch(err => fetch(`${that.parent.api}/log?type=${err.name}&message=${err.message}`))
    // }
}
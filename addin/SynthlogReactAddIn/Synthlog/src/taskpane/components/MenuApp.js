import * as React from 'react';
import 'isomorphic-fetch';
import { Button, ButtonType } from 'office-ui-fabric-react';


export default class MenuApp extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'https://localhost:3001/api';
    this.state = {
        init: false,
        nb_calls: 0,
    };
  }


  render() {
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
            <a href="taskpane.html">Dev Taskpane</a><br/>
            <a href="mercs.html">Mercs model</a><br/>
            <a href="tacle.html">TaCLe</a><br/>
            <Button className='normal-button' buttonType={ButtonType.hero} onClick={this.increaseCount.bind(this)}>Increase count</Button>
            <p>
              Count = {this.state.nb_calls}
            </p>
        </div>
      </div>
    );
  }

  increaseCount(){
    this.setState({nb_calls : this.state.nb_calls + 1})
  }
}
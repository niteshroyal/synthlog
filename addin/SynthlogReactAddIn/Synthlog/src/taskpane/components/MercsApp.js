import * as React from 'react';
import 'isomorphic-fetch';
import SynthAppParent from './SynthAppParent';


export default class MercsApp extends SynthAppParent {
  constructor(props, context) {
    super(props, context);
    this.state = {
    };
  }


  render() {
    return (
      <div id='main' className='ms-welcome'>
        <div id="info">
            <p>
                Mercs
            </p>
        </div>
      </div>
    );
  }
}
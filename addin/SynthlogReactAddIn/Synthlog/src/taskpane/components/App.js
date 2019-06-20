import * as React from 'react';
import { Button, ButtonType } from 'office-ui-fabric-react';
import Header from './Header';
import HeroList, { HeroListItem } from './HeroList';
import Progress from './Progress';
import 'isomorphic-fetch';

export default class App extends React.Component {
  constructor(props, context) {
    super(props, context);
    this.api = 'http://localhost:3001/api';
    this.state = {
      listItems: [],
      greeting: 'lol',
      init: false,
      init_error: ''
    };
  }

  componentDidMount() {
    fetch(`${this.api}/init`)
      .then(response => response.json())
      .then(json => this.setState(json))
      .catch(e => this.setState({init_error: e.toString()}))

    fetch(`${this.api}/greeting?name=SynthLog`)
      .then(response => response.json())
      .then(json => this.setState(json))
      .catch(e => this.setState({greeting: e.toString()}))
  }

  click = async () => {
    try {
      await Excel.run(async context => {
        /**
         * Insert your Excel code here
         */
        const range = context.workbook.getSelectedRange();

        // Read the range address
        range.load("address");

        // Update the fill color
        range.format.fill.color = "yellow";

        await context.sync();
        console.log(`The range address was ${range.address}.`);
      });
    } catch (error) {
      console.error(error);
    }
  }

  render() {
    const {
      title,
      isOfficeInitialized,
    } = this.props;

    if (!isOfficeInitialized) {
      return (
        <Progress
          title={title}
          logo='assets/logo-filled.png'
          message='Please sideload your addin to see app body.'
        />
      );
    }

    return (
      <div className='ms-welcome'>
        <Header logo='assets/logo-filled.png' title={this.props.title} message={this.state.greeting} />
        <HeroList message={this.state.init.toString()} items={this.state.listItems}>
          <p className='ms-font-l'>Modify the source files, then click <b>Run</b>.</p>
          <Button className='ms-welcome__action' buttonType={ButtonType.hero} iconProps={{ iconName: 'ChevronRight' }} onClick={this.click}>Run</Button>
        </HeroList>
      </div>
    );
  }
}

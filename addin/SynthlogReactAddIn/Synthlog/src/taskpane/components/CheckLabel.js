import * as React from 'react';
import Boolean from './Boolean';

export default class CheckLabel extends React.Component {
    render() {
        const {
            message,
            boolean
        } = this.props;
        
        return (
            <p>
                { message } <Boolean boolean={ boolean } />
            </p>
        );
    }
}

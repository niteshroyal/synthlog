import * as React from 'react';

export default class Boolean extends React.Component {
    render() {
        const {
            boolean
        } = this.props;

        var symbol = String.fromCharCode(10060);
        var css_class = "wrong";
        if (boolean) {
          symbol = String.fromCharCode(10003);
          css_class = "ok";
        }

        return (
            <span class={css_class}>{ symbol }</span>
        );
    }
}

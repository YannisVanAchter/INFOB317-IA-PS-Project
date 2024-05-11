import React from 'react';

import './modal.css';

function Modal(props: any) {
    return (
        <div className='modal-components'>
            <div className={`modal-components-content ${props.className}`}>
                {props.children}
            </div>
        </div>
    )
}

export { Modal };
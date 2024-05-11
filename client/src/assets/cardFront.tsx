import React from 'react';
import { CardValue } from '../types/game';


const CardFront = ({ number, className = '' }: {number: CardValue, className?: string}) => {
    // Contenu du SVG initial
    const svgContent = `
    <svg viewBox="280.998 87.675 280 420" preserveAspectRatio="xMidYMid meet">
        <g opacity="1">
            <path d="M280.998 87.675L560.998 87.675L560.998 507.675L280.998 507.675L280.998 87.675Z" fill="#f1d244" fill-rule="nonzero" opacity="1" stroke="none"/>
            <path d="M280.998 87.675L560.998 87.675L560.998 507.675L280.998 507.675L280.998 87.675Z" fill="none" opacity="1" stroke="#000000" stroke-linecap="butt" stroke-linejoin="round" stroke-width="10"/>
            
            <!-- Add number here --> <!-- Line added by Yannis Van Achter -->
            
        </g>
        <path d="M291.998 98.675L291.998 201.363C331.207 195.87 374.04 183.484 412.716 165.831C452.927 147.478 486.422 124.477 510.123 98.675L291.998 98.675ZM549.998 375.675C543.639 378.343 536.981 380.968 529.654 383.581C523.353 385.828 516.978 387.957 508.466 390.738C506.173 391.487 487.511 397.606 486.935 397.8C425.845 418.392 366.328 431.896 311.248 436.363C304.954 436.873 298.598 437.053 291.998 437.05L291.997 496.675L549.998 496.675L549.998 375.675Z" fill="#000000" fill-rule="nonzero" opacity="1" stroke="none"/>
    </svg>
    `;

    // Ajoutez le nombre au contenu du SVG
    const UpdatedSvgContent = svgContent.replace('<!-- Add number here -->', `<text x="150%" y="90%" text-anchor="middle" font-size="250">${number}</text>`);

    return (
        <div className={`${className}`} dangerouslySetInnerHTML={{ __html: UpdatedSvgContent }} />
        // <UpdatedSvgContent />
    );
};

export default CardFront;
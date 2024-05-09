import React from 'react';

type cardValue = -3 | -2 | -1 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12

const CardFront = ({ number, className = '' }: {number: cardValue, className?: string}) => {
    // Contenu du SVG initial
    const svgContent = `
    <!-- All credits to Mariya-Laurel Ivakhnenko that created this SVG -->
    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
    <!-- Created with Vectornator (http://vectornator.io/) -->
    <svg height="100%" stroke-miterlimit="10" style="fill-rule:nonzero;clip-rule:evenodd;stroke-linecap:round;stroke-linejoin:round;" version="1.1" viewBox="0 0 841.995 595.35" width="100%" xml:space="preserve" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
    <defs/>
    <g id="Calque-1">
    <g opacity="1">
    <path d="M280.998 87.675L560.998 87.675L560.998 507.675L280.998 507.675L280.998 87.675Z" fill="#f1d244" fill-rule="nonzero" opacity="1" stroke="none"/>
    <path d="M280.998 87.675L560.998 87.675L560.998 507.675L280.998 507.675L280.998 87.675Z" fill="none" opacity="1" stroke="#000000" stroke-linecap="butt" stroke-linejoin="round" stroke-width="10"/>
    
    <!-- Add number here --> <!-- Line added by Yannis Van Achter -->
    
    </g>
    <path d="M291.998 98.675L291.998 201.363C331.207 195.87 374.04 183.484 412.716 165.831C452.927 147.478 486.422 124.477 510.123 98.675L291.998 98.675ZM549.998 375.675C543.639 378.343 536.981 380.968 529.654 383.581C523.353 385.828 516.978 387.957 508.466 390.738C506.173 391.487 487.511 397.606 486.935 397.8C425.845 418.392 366.328 431.896 311.248 436.363C304.954 436.873 298.598 437.053 291.998 437.05L291.997 496.675L549.998 496.675L549.998 375.675Z" fill="#000000" fill-rule="nonzero" opacity="1" stroke="none"/>
    </g>
    </svg>
    `;

    // Ajoutez le nombre au contenu du SVG
    const updatedSvgContent = svgContent.replace('<!-- Add number here -->', `<text x="48.5%" y="62.5%" text-anchor="middle" font-size="250">${number}</text>`);

    return (
        <div className={`${className}`} dangerouslySetInnerHTML={{ __html: updatedSvgContent }} />
    );
};

export default CardFront;
export type { cardValue };

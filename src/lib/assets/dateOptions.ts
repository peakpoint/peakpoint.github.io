
export const dateOptions: Intl.DateTimeFormatOptions = {
    weekday: 'short',
    year: 'numeric',
    month: 'long',
    day: 'numeric',
}

export function formatDate(d: string) {
    return new Date(`${d} 00:00:00 EDT`).toLocaleDateString('en-US', dateOptions)
}

def buildErrorPacket(res):
    return {
        'error': {
            "status": res.status_code,
            "message": res.reason,
            "description": res.text
        }
    }
